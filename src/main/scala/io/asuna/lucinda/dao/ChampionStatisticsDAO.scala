package io.asuna.lucinda.dao

import io.asuna.lucinda.FutureUtil
import io.asuna.lucinda.statistics.StatisticsAggregator
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics
import io.asuna.proto.service_vulgate.VulgateGrpc.Vulgate
import redis.RedisClient
import io.asuna.lucinda.database.LucindaDatabase
import scala.concurrent.{ ExecutionContext, Future }

/**
  * String representation of champ statistics. Used for a redis key.
  */
case class ChampionStatisticsId(
  // TODO(igm): support queue type
  tiers: Set[Int], patch: String, region: Region, enemy: Int
) {
  def keyify: String = upickle.default.write(this)
}

class ChampionStatisticsDAO(db: LucindaDatabase, redis: RedisClient)(implicit ec: ExecutionContext) {

  /**
    * Gets a ChampionStatistics object with Redis caching. We cache for 15 minutes. TODO(igm): make this duration configurable
    */
  def get(champions: Set[Int], tiers: Set[Int], patch: String, region: Region, enemy: Int = -1): Future[ChampionStatistics] = {
    import scala.concurrent.duration._

    val id = ChampionStatisticsId(tiers, patch, region, enemy)
    val key = id.keyify
    redis.get(key) flatMap {
      // If the key is found, we shall parse it
      case Some(bytes) => Future(ChampionStatistics.parseFrom(bytes.toArray[Byte]))

      // If the key is not found, recalculate it and write it
      case None => for {
        stats <- forceGet(champions, tiers, patch, region, enemy)
        result <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats
    }
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
  private def forceGet(champions: Set[Int], tiers: Set[Int], patch: String, region: Region, enemy: Int): Future[ChampionStatistics] = {
    // A lot goes on in this function, especially since we're dealing with Futures.
    // I'll try to explain every step in detail.

    // First, we're going to iterate over every Role. This is a given --
    // the ChampionStatistics returns one Statistics object for each role.
    val statsFuts = Role.values.map { role =>
      // Here, we build the Set[MatchFilters] for every champion.
      // This is of type Map[Int, Set[MatchFilters]].
      val filtersMap = champions.map {champ =>
        (champ, StatisticsAggregator.buildFilterSet(champ, patch, tiers, region, role, enemy))
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
      // object for each value. Thus we end up with a Future[Statistics].
      sumsMapFut.map { sumsMap =>
        StatisticsAggregator.makeStatistics(role, sumsMap)
      }
    }

    // Now that we have a List[Future[Statistics]], we'll again unwrap the Future
    // to create a Future[ChampionStatistics], and we are done.
    Future.sequence(statsFuts).map(s => ChampionStatistics(statistics = s))
  }

}
