package io.asuna.lucinda.dao

import io.asuna.lucinda.filters.MatchFilterSet
import io.asuna.lucinda.VulgateHelpers
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import io.asuna.lucinda.FutureUtil
import io.asuna.lucinda.statistics.{ StatisticsAggregator, StatisticsCombiner }
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.range.{ PatchRange, TierRange }
import io.asuna.proto.service_lucinda.LucindaRpc.GetStatisticsResponse.RoleStatistics
import io.asuna.proto.service_vulgate.VulgateGrpc.Vulgate
import io.asuna.proto.service_vulgate.VulgateRpc
import io.asuna.proto.vulgate.VulgateData
import redis.RedisClient
import io.asuna.lucinda.database.LucindaDatabase
import scala.concurrent.{ ExecutionContext, Future }

/**
  * String representation of champ statistics. Used for a redis key.
  */
case class ChampionStatisticsId(
  // TODO(igm): support queue type
  tiers: Set[Int], patch: String, region: Region, role: Role, enemy: Int
)

class ChampionStatisticsDAO(db: LucindaDatabase, redis: RedisClient)(implicit ec: ExecutionContext) {

  /**
    * Gets a RoleStatistics object.
    */
  def getWithRoles(factors: AggregationFactors, region: Region, forceRefresh: Boolean = false): Future[Seq[RoleStatistics]] = {
    // TODO(igm): locale
    for {
      roleStats <- Future.sequence(
        (Role.values zip factors.patches) map { case (role, patch) =>
          get(factors.champions.toSet, factors.tiers.toSet, patch, region, role, forceRefresh = forceRefresh).map((role, _))
        }
      )
    } yield roleStats.map { case (role, statistics) =>
      RoleStatistics(
        role = role,
        statistics = Some(statistics)
      )
    }
  }

  /**
    * Gets a ChampionStatistics object with Redis caching. We cache for 15 minutes. TODO(igm): make this duration configurable
    */
  def get(
    champions: Set[Int], tiers: Set[Int], patch: String, region: Region,
    role: Role, enemy: Int = -1, reverse: Boolean = false, forceRefresh: Boolean = false
  ): Future[ChampionStatistics] = {
    import scala.concurrent.duration._

    val id = ChampionStatisticsId(tiers, patch, region, role, enemy)
    val key = id.toString

    val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

    redisResult flatMap {
      // If the key is not found, recalculate it and write it
      case None => for {
        stats <- forceGet(champions, tiers, patch, region, role, enemy, reverse)
        _ <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats

      // If the key is found, we shall parse it
      case Some(bytes) => Future.successful(ChampionStatistics.parseFrom(bytes.toArray[Byte]))
    }
  }

  /**
    * Runs get across multiple patches and aggregates into one ChampionStatistics object.
    */
  def getForPatches(
    champions: Set[Int], tiers: Set[Int], patch: Set[String], region: Region,
    role: Role, enemy: Int = -1, reverse: Boolean = false, forceRefresh: Boolean = false
  ): Future[ChampionStatistics] = {
    for {
      statsList <- Future.sequence(
        patch.map(
          get(champions, tiers, _, region, role, enemy, reverse, forceRefresh = forceRefresh)))
    } yield StatisticsCombiner.combine(statsList.toList)
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
    champions: Set[Int], tiers: Set[Int], patch: String, region: Region,
    role: Role, enemy: Int, reverse: Boolean
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
    val sumsMapFut = FutureUtil.sequenceMap(sumsMapFuts)

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
