package asuna.lucinda.dao

import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.MatchFiltersSpace
import asuna.proto.league.alexandria.StoredAllChampionStatistics
import asuna.proto.league.alexandria.rpc.UpsertAllChampionStatisticsRequest
import asuna.proto.league.lucinda.AllChampionStatisticsKey
import scala.concurrent.{ExecutionContext, Future}

import asuna.lucinda.LucindaConfig
import asuna.lucinda.statistics.{ ChangeMarker, StatisticsAggregator }
import asuna.lucinda.statistics.FilterChampionsHelpers._
import asuna.lucinda.statistics.StatisticsCombiner._
import asuna.proto.league.{ MatchFilters, Queue, Region, Role, Tier }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc.GetSumRequest
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.vulgate.AggregationFactors
import cats.implicits._

class AllChampionStatisticsDAO(
  config: LucindaConfig, alexandria: Alexandria)(implicit ec: ExecutionContext) {

  /**
   * Fetches a AllChampionStatistics.Results object.
   */
  def getResults(
    allChampions: Set[Int],
    prevPatch: Option[String],
    patches: Set[String],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    minPickRate: Double = 0
  ): Future[AllChampionStatistics.Results] = {
    for {
      statistics <- get(
        allChampions = allChampions,
        patches = patches,
        prevPatch = prevPatch,
        tiers = tiers,
        regions = regions,
        roles = roles,
        queues = queues,
        enemies = enemies,
        reverse = false
      )
    } yield {
      // Get the results object
      val results = statistics.results.getOrElse(AllChampionStatistics.Results())

      // Find the champions that satisfy the min play rate
      val pickRates = results.derivatives
        .map(_.picks.mapValues(_.value)).orEmpty
      val champs = pickRates.filter {
        case (champ, pickRate) => pickRate >= minPickRate
      }.keys

      // Filter maps for keys that contain the champion
      results.filterChampions(champs.toSet)
    }
  }

  /**
    * Gets a AllChampionStatistics object with caching.
    */
  def get(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    prevPatch: Option[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ): Future[AllChampionStatistics] = {
    prevPatch match {
      case Some(patch) => {
        val prevFut = bareGetSingle(
          allChampions,
          tiers,
          Set(patch),
          regions,
          roles,
          queues,
          enemies,
          reverse
        )
        val curFut = bareGetSingle(
          allChampions,
          tiers,
          patches,
          regions,
          roles,
          queues,
          enemies,
          reverse
        )
        (prevFut |@| curFut).map { (prev, cur) =>
          ChangeMarker.mark(cur, prev)
        }
      }
      case None => {
        bareGetSingle(
          allChampions, tiers, patches, regions,
          roles, queues, enemies, reverse
        )
      }
    }
  }

  /**
    * Gets a AllChampionStatistics object with caching.
    * We cache for 15 minutes. TODO(igm): make this duration configurable
    */
  private def bareGetSingle(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ): Future[AllChampionStatistics] = {
    val key = keyFromSets(tiers, patches, regions, roles, enemies, queues, reverse)

    // Fetch champ statistics from the cache
    alexandria.getAllChampionStatistics(key).map(_.some) flatMap {
      // If the key is found, we shall parse it
      // TODO(igm): if TS time remaining is low enough, refetch
      case Some(StoredAllChampionStatistics(Some(data), Some(ts))) => {
        val ms = ts.seconds * 1000 + (ts.nanos / 1E6)
        if (System.currentTimeMillis - ms > config.allChampionStatisticsCacheExpiry.toMillis) {
          // Run this cache update in the background.
          // TODO(igm): convert to task and submit to task queue with known thread pool size
          forceGetWithCacheUpdate(
            key,
            allChampions,
            tiers,
            patches,
            regions,
            roles,
            enemies,
            queues,
            reverse
          )
        }
        Future.successful(data)
      }

      // If the key is not found (or some other bs), recalculate it and write it
      case _ => forceGetWithCacheUpdate(
        key,
        allChampions,
        tiers,
        patches,
        regions,
        roles,
        enemies,
        queues,
        reverse
      )
    }
  }

  private def forceGetWithCacheUpdate(
    key: AllChampionStatisticsKey,
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue],
    reverse: Boolean
  ): Future[AllChampionStatistics] = {
    for {
      stats <- forceGet(
        allChampions,
        tiers,
        patches,
        regions,
        roles,
        enemies,
        queues,
        reverse
      )

      // Insert back to alexandria
      req = UpsertAllChampionStatisticsRequest(
        key = key.some,
        value = stats.some
      )
      _ <- alexandria.upsertAllChampionStatistics(req)
    } yield stats
  }

  /**
    *  This function derives a AllChampionStatistics object from a patch, a set of tiers, a region, and an enemy.
    *
    *  An overview of the steps to do this is as follows:
    *  1. Find filters for each champion. (see buildFilterSet)
    *  2. Convert these filters into MatchSums using the database.
    *  3. Build the Statistics objects from the raw MatchSums. (see makeStatistics)
    *
    *  This does not take caching into account.
    */
  private def forceGet(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue],
    reverse: Boolean
  ): Future[AllChampionStatistics] = {
    // A lot goes on in this function, especially since we're dealing with Futures.
    // I'll try to explain every step in detail.

    // Here, we build the MatchFiltersSpace for every champion.
    val filtersMap: Map[Int, MatchFiltersSpace] = allChampions
      .map(c => (c, c)).toMap
      .mapValues { champ =>
        val basis = MatchFilterSpaceHelpers.generate(
          Set(champ), patches, tiers, regions, enemies, roles, queues
        )
        if (reverse) {
          basis.copy(championIds = basis.enemyIds, enemyIds = basis.championIds)
        } else {
          basis
        }
      }

    // Next, we'll compute the MatchSums. This is where the function is no longer
    // pure, and we make a database call. (Note that since we're using futures,
    // no database call is made at the time of execution.) This returns a
    // Map[Int, Future[MatchSum]].
    val sumsMapFuts = filtersMap
      .mapValues(space => alexandria.getSum(GetSumRequest(space = space.some)))

    // Finally, we'll map over the values of this map to generate a Statistics
    // object for each value. Thus we end up with a Future[AllChampionStatistics],
    // and we are done.
    sumsMapFuts.sequence.map { sumsMap =>
      StatisticsAggregator.makeStatistics(sumsMap)
    }
  }

  private def invertFilters(filters: MatchFilters): MatchFilters = {
    filters.copy(championId = filters.championId, enemyId = filters.enemyId)
  }

  private def keyFromSets(
    tiers: Set[Tier],
    patch: Set[String],
    region: Set[Region],
    role: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue],
    reverse: Boolean
  ): AllChampionStatisticsKey = AllChampionStatisticsKey(
    tiers = tiers.toSeq,
    patches = patch.toSeq,
    regions = region.toSeq,
    roles = role.toSeq,
    enemyIds = enemies.toSeq,
    queues = queues.toSeq,
    reverse = reverse
  )

}
