package io.asuna.lucinda.dao

import io.asuna.lucinda.CPReducedMatchFilterSpace
import io.asuna.lucinda.MatchFilterSpace
import io.asuna.lucinda.VulgateHelpers
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import io.asuna.lucinda.FutureUtil
import io.asuna.lucinda.statistics.{ StatisticsAggregator, StatisticsCombiner }
import io.asuna.proto.enums.{ QueueType, Region, Role }
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
import cats.implicits._
import StatisticsCombiner._

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
  def getWithRoles(
    factors: AggregationFactors,
    region: Region,
    queues: Set[QueueType],
    forceRefresh: Boolean = false
  ): Future[Seq[RoleStatistics]] = {
    val spaces = MatchFilterSpace(factors, region, queues)
    for {
      roleStats <- FutureUtil.sequenceMap(spaces.mapValues(get(_)))
    } yield roleStats.map { case (role, statistics) =>
      RoleStatistics(
        role = role,
        statistics = Some(statistics.values.toList.combineAll)
      )
    }.toSeq
  }

  /**
    * Gets a ChampionStatistics object with Redis caching. We cache for 15 minutes. TODO(igm): make this duration configurable
    * This returns a map of patch to ChampionStatistics object.
    */
  def get(
    spaces: Map[String, Map[Int, CPReducedMatchFilterSpace]],
    reverse: Boolean = false,
    forceRefresh: Boolean = false
  ): Future[Map[String, ChampionStatistics]] = {
    Future.sequence {
      spaces.map { case (patch, subspaces) =>
        getPatch(subspaces, reverse, forceRefresh) map { res =>
          (patch, res)
        }
      }
    }.map(_.toMap)
  }

  /**
    * Gets a single space.
    */
  private[this] def getPatch(
    spaces: Map[Int, CPReducedMatchFilterSpace],
    reverse: Boolean,
    forceRefresh: Boolean
  ): Future[ChampionStatistics] = {
    import scala.concurrent.duration._

    val key = spaces.toString

    val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

    redisResult flatMap {
      // If the key is not found, recalculate it and write it
      case None => for {
        stats <- forceGet(spaces, reverse)
        _ <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats

      // If the key is found, we shall parse it
      case Some(bytes) => Future.successful(ChampionStatistics.parseFrom(bytes.toArray[Byte]))
    }
  }

  /**
    *  This function derives a ChampionStatistics object.
    *
    *  An overview of the steps to do this is as follows:
    *  1. Find filters for each champion. (see buildFilterSet)
    *  2. Convert these filters into MatchSums using the database.
    *  3. Build the Statistics objects from the raw MatchSums. (see makeStatistics)
    *
    *  This does not take caching into account.
    */
  private[this] def forceGet(
    spaces: Map[Int, CPReducedMatchFilterSpace],
    reverse: Boolean
  ): Future[ChampionStatistics] = {
    // This should always exist. If not, we've done something horribly wrong.
    val role = spaces.values.head.role

    // A lot goes on in this function, especially since we're dealing with Futures.
    // I'll try to explain every step in detail.

    // Here, we build the Set[MatchFilters] for every champion.
    // This is of type Map[Int, Set[MatchFilters]].
    // Now MatchFilterSpace does most of the heavy lifting.
    // This function used to be fucked.
    val maybeTradingSpaces = if (reverse) {
      spaces.mapValues(_.inverse)
    } else {
      spaces
    }
    val filtersMap = maybeTradingSpaces.mapValues(_.toFilterSet)

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
