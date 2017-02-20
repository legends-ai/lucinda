package asuna.lucinda.dao

import asuna.proto.league.alexandria.StoredStatistics
import asuna.proto.league.alexandria.rpc.UpsertStatisticsRequest
import asuna.proto.league.lucinda.StatisticsKey
import scala.concurrent.{ ExecutionContext, Future }

import asuna.lucinda.matches.MinPlayRateDecorator
import asuna.lucinda.filters.MatchFilterSet
import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league.{ Queue, Region, Role, Tier }
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc.GetSumRequest
import cats.implicits._

class StatisticsDAO(
  alexandria: Alexandria, allChampionStatisticsDAO: AllChampionStatisticsDAO
)(implicit ec: ExecutionContext) {

  def get(
    allChampions: Set[Int],
    patches: Set[String],
    lastFivePatches: Seq[String],
    prevPatches: Map[String, String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[Statistics] = {
    val key = keyFromSets(
      patches = patches,
      champions = champions,
      tiers = tiers,
      regions = regions,
      roles = roles,
      enemies = enemies,
      queues = queues
    )

    // Fetch champ statistics from the cache
    val cacheResult = if (forceRefresh) {
      Future.successful(None)
    } else {
      alexandria.getStatistics(key)
    }

    val statsFut = cacheResult flatMap {
      // If the key is found, we shall parse it
      // TODO(igm): if TS time remaining is low enough, refetch
      case StoredStatistics(Some(data), _) => Future.successful(data)

      // If the key is not found or we're using force refresh, recalculate it and write it
      case _ => for {
        stats <- forceGet(
          allChampions = allChampions,
          patches = patches,
          lastFivePatches = lastFivePatches,
          prevPatches = prevPatches,
          champions = champions,
          tiers = tiers,
          regions = regions,
          roles = roles,
          enemies = enemies,
          queues = queues,
          forceRefresh = forceRefresh
        )

        // Insert back to alexandria
        req = UpsertStatisticsRequest(
          key = key.some,
          value = stats.some
        )
        _ <- alexandria.upsertStatistics(req)
      } yield stats
    }

    statsFut.map { stats =>
      MinPlayRateDecorator.decorate(minPlayRate, stats)
    }
  }

  /**
    * Fetches and aggregates information about a single champion.
    *
    * @param patches -- The patches in the patch range we are considering.
    * @param lastFivePatches -- The last five patches of the game.
    */
  private def forceGet(
    allChampions: Set[Int],
    patches: Set[String],
    lastFivePatches: Seq[String],
    prevPatches: Map[String, String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue],
    forceRefresh: Boolean
  ): Future[Statistics] = {
    // First, let's get per-role sums.
    val byRoleFilters = (Role.values.toSet - Role.UNDEFINED_ROLE).map { someRole =>
      (someRole, MatchFilterSet(champions, patches, tiers, regions, enemies, Set(someRole), queues).toFilterSet)
    }.toMap
    val byRoleFuts = byRoleFilters
      .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq)))

    for {
      byRole <- byRoleFuts.sequence

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      allStatsPatches = patches union lastFivePatches.toSet
      allStatsFuts = allStatsPatches.toList.map { patch =>
        allChampionStatisticsDAO.getSingle(
          allChampions, tiers, Set(patch), prevPatches.get(patch), regions, roles, queues, enemies, forceRefresh
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      allStats <- allStatsFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = lastFivePatches.map { patch =>
        (patch, MatchFilterSet(champions, Set(patch), tiers, regions, enemies, roles, queues).toFilterSet)
      }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters
        .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq))).sequence

    } yield StatisticsGenerator.makeStatistics(
      champions = champions,
      allStats = allStats,
      roles = roles,
      byRole = byRole,
      byPatch = byPatch,
      patches = patches
    )
  }

  private def keyFromSets(
    patches: Set[String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue]
  ): StatisticsKey = StatisticsKey(
    championIds = champions.toSeq,
    patches = patches.toSeq,
    tiers = tiers.toSeq,
    regions = regions.toSeq,
    roles = roles.toSeq,
    enemyIds = enemies.toSeq,
    queues = queues.toSeq
  )


}
