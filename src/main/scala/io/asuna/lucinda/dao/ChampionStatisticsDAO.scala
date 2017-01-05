package io.asuna.lucinda.dao

import io.asuna.lucinda.statistics.StatisticsCombiner._
import io.asuna.lucinda.statistics.{ ChangeMarker, StatisticsAggregator }
import io.asuna.proto.enums.QueueType
import scala.concurrent.{ExecutionContext, Future}

import cats.implicits._
import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.lucinda.filters.MatchFilterSet
import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import redis.RedisClient

/**
  * String representation of champ statistics. Used for a redis key.
  */
case class ChampionStatisticsId(
  tiers: List[Int],
  patch: String,
  region: Region,
  role: Role,
  enemy: Int,
  queues: List[QueueType]
)

object ChampionStatisticsId {

  def fromSets(
    tiers: Set[Int],
    patch: String,
    region: Region,
    role: Role,
    enemy: Int,
    queues: Set[QueueType]
  ): ChampionStatisticsId = ChampionStatisticsId(
    tiers = tiers.toList.sorted,
    patch = patch,
    region = region,
    role = role,
    enemy = enemy,
    queues = queues.toList.sortBy(_.value)
  )

}

class ChampionStatisticsDAO(db: LucindaDatabase, redis: RedisClient)(implicit ec: ExecutionContext) {

  /**
    * Get uses AggregationFactors to fetch.
    */
  def get(
    factors: AggregationFactors,
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int = -1,
    reverse: Boolean = false,
    forceRefresh: Boolean = false
  ): Future[ChampionStatistics] = {
    getForPatches(
      champions = factors.champions.toSet,
      tiers = factors.tiers.toSet,
      patches = factors.patches.toSet,
      prevPatches = factors.prevPatches,
      region = region,
      role = role,
      queues = queues,
      enemy = enemy,
      reverse = reverse,
      forceRefresh = forceRefresh
    )
  }

  /**
    * Gets a ChampionStatistics object with Redis caching. We cache for 15 minutes. TODO(igm): make this duration configurable
    */
  def getSingle(
    champions: Set[Int],
    tiers: Set[Int],
    patch: String,
    prevPatch: Option[String],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int = -1,
    reverse: Boolean = false,
    forceRefresh: Boolean = false
  ): Future[ChampionStatistics] = {
    import scala.concurrent.duration._
    role match {
      case Role.UNDEFINED_ROLE => {
        (Role.values.toSet - Role.UNDEFINED_ROLE).toList.map { theRole =>
          getSingle(champions, tiers, patch, prevPatch, region, theRole, queues, enemy, reverse, forceRefresh)
        }.combineAll
      }

      case _ => {
        if (!prevPatch.isDefined) {
          forceGet(champions, tiers, patch, region, role, enemy, reverse)
        } else {
          val id = ChampionStatisticsId.fromSets(tiers, patch, region, role, enemy, queues)
          val key = id.toString

          val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

          redisResult flatMap {
            // If the key is not found, recalculate it and write it
            case None => for {
              // TODO(igm): don't force get the previous patch, but instead read it back from redis
              prev <- forceGet(champions, tiers, prevPatch.get, region, role, enemy, reverse)
              stats <- forceGet(champions, tiers, patch, region, role, enemy, reverse)
              _ <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
            } yield ChangeMarker.mark(stats, prev)

            // If the key is found, we shall parse it
            case Some(bytes) => Future.successful(ChampionStatistics.parseFrom(bytes.toArray[Byte]))
          }
        }
      }
    }
  }

  /**
    * Runs get across multiple patches and aggregates into one ChampionStatistics object.
    */
  private[this] def getForPatches(
    champions: Set[Int],
    tiers: Set[Int],
    patches: Set[String],
    prevPatches: Map[String, String],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int = -1,
    reverse: Boolean = false,
    forceRefresh: Boolean = false
  ): Future[ChampionStatistics] = {
    for {
      statsList <- patches.toList.map(patch =>
        getSingle(
          champions, tiers, patch, prevPatches.get(patch), region, role, queues, enemy, reverse, forceRefresh = forceRefresh
        )).sequence
    } yield statsList.toList.combineAll
  }

  /**
    *  This function derives a ChampionStatistics object from a patch, a set of tiers, a region, and an enemy.
    *
    *  An overview of the steps to do this is as follows:
    *  1. Find filters for each champion. (see buildFilterSet)
    *  2. Convert these filters into MatchSums using the database.
    *  3. Build the Statistics objects from the raw MatchSums. (see makeStatistics)
    *
    *  This does not take caching into account.
    */
  private def forceGet(
    champions: Set[Int],
    tiers: Set[Int],
    patch: String,
    region: Region,
    role: Role,
    enemy: Int,
    reverse: Boolean
  ): Future[ChampionStatistics] = {
    // A lot goes on in this function, especially since we're dealing with Futures.
    // I'll try to explain every step in detail.

    // Here, we build the Set[MatchFilters] for every champion.
    // This is of type Map[Int, Set[MatchFilters]].
    val filtersMap = champions.map { champ =>
      val basis = MatchFilterSet(champ, patch, tiers, region, enemy, role)
      val nextSet = if (reverse) {
        basis.inverse
      } else {
        basis
      }
      (champ, nextSet.toFilterSet)
    }.toMap

    // Next, we'll compute the MatchSums. This is where the function is no longer
    // pure, and we make a database call. (Note that since we're using futures,
    // no database call is made at the time of execution.) This returns a
    // Map[Int, Future[MatchSum]].
    val sumsMapFuts = filtersMap.mapValues(filters => db.matchSums.sum(filters))

    // Next, we'll extract the Future from the value using some map magic.
    // Thus we end up with a Future[Map[Int, MatchSum]].
    val sumsMapFut = sumsMapFuts.sequence

    // Finally, we'll map over the values of this map to generate a Statistics
    // object for each value. Thus we end up with a Future[ChampionStatistics],
    // and we are done.
    sumsMapFut.map { sumsMap =>
      StatisticsAggregator.makeStatistics(role, sumsMap)
    }
  }

  private def invertFilters(filters: MatchFilters): MatchFilters = {
    filters.copy(championId = filters.championId, enemyId = filters.enemyId)
  }

}
