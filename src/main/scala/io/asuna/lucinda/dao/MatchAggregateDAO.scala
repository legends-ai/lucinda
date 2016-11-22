package io.asuna.lucinda.dao

import io.asuna.lucinda.FutureUtil
import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.lucinda.matches.MatchAggregator
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion.MatchAggregate
import redis.RedisClient
import scala.concurrent.{ ExecutionContext, Future }

case class MatchAggregateId(
  // TODO(igm): support queue type
  // TODO(igm): don't cache based on minPlayRate -- calculate on the fly
  champion: Int, tiers: Set[Int], region: Region, role: Role, enemy: Int = -1, minPlayRate: Double
) {
  def keyify: String = upickle.default.write(this)
}

class MatchAggregateDAO(db: LucindaDatabase, redis: RedisClient, statistics: ChampionStatisticsDAO)(implicit ec: ExecutionContext) {

  def get(
    champions: Set[Int], patches: Set[String], lastFivePatches: Set[String],
    champion: Int, tiers: Set[Int], region: Region, role: Role, enemy: Int = -1, minPlayRate: Double
  ): Future[MatchAggregate] = {
    import scala.concurrent.duration._

    val id = MatchAggregateId(champion, tiers, region, role, enemy, minPlayRate)
    val key = id.keyify
    redis.get(key) flatMap {
      // If the key is found, we shall parse it
      case Some(bytes) => Future(MatchAggregate.parseFrom(bytes.toArray[Byte]))

      // If the key is not found, recalculate it and write it
      case None => for {
        stats <- forceGet(
          champions, patches, lastFivePatches,
          champion, tiers, region, role, enemy, minPlayRate
        )
        // TODO(igm): make this time configurable.
        result <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats
    }
  }

  /**
    * Fetches and aggregates information about a single champion.
    *
    * @param patches -- The patches in the patch range we are considering.
    * @param lastFivePatches -- The last five patches of the game.
    */
  private def forceGet(
    champions: Set[Int], patches: Set[String], lastFivePatches: Set[String],
    champion: Int, tiers: Set[Int], region: Region, role: Role, enemy: Int = -1, minPlayRate: Double
  ): Future[MatchAggregate] = {
    // First, let's retrieve all stats for this combination.
    // TODO(igm): use the cache when it is implemented
    val allStatsFuts = patches.map { patch =>
      statistics.get(champions, tiers, patch, region, role, enemy).map((patch, _))
    }

    // This future contains an element of the form Map[String, ChampionStatistics]
    // where key is the patch and value is the stats.
    val allStatsFut = Future.sequence(allStatsFuts).map(_.toMap)

    // Next, let's get per-role sums.
    val byRoleFilters = Role.values.map { someRole =>
      (role, patches.flatMap {
         patch => MatchAggregator.buildFilters(champion, patch, tiers, region, enemy, someRole)
       })
    }.toMap
    val byRoleFut = FutureUtil.sequenceMap(byRoleFilters.mapValues(filters => db.matchSums.sum(filters)))

    // Next, let's get per-patch sums.
    val byPatchFilters = lastFivePatches.map { patch =>
      (patch, MatchAggregator.buildFilters(champion, patch, tiers, region, enemy, role))
    }.toMap
    val byPatchFut = FutureUtil.sequenceMap(byPatchFilters.mapValues(filters => db.matchSums.sum(filters)))

    // Finally, we'll execute and build everything.
    for {
      allStats <- allStatsFut
      byRole <- byRoleFut
      byPatch <- byPatchFut
    } yield MatchAggregator.makeAggregate(champion, minPlayRate, allStats, byRole, byPatch)
  }

}
