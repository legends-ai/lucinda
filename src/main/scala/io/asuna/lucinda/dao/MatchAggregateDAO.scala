package io.asuna.lucinda.dao

import io.asuna.lucinda.filters.MatchFilterSet
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
  patches: Set[String],
  lastFivePatches: Seq[String],
  champion: Int,
  tiers: Set[Int],
  region: Region,
  role: Role,
  enemy: Int,
  minPlayRate: Double
)

class MatchAggregateDAO(db: LucindaDatabase, redis: RedisClient, statistics: ChampionStatisticsDAO)(implicit ec: ExecutionContext) {

  def get(
    champions: Set[Int],
    patches: Set[String],
    lastFivePatches: Seq[String],
    champion: Int,
    tiers: Set[Int],
    region: Region,
    role: Role,
    enemy: Int = -1,
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[MatchAggregate] = {
    import scala.concurrent.duration._

    val id = MatchAggregateId(
      patches = patches,
      lastFivePatches = lastFivePatches,
      champion = champion,
      tiers = tiers,
      region = region,
      role = role,
      enemy = enemy,
      minPlayRate = minPlayRate
    )
    val key = id.toString

    val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

    redisResult flatMap {
      // If the key is not found or we're using force refresh, recalculate it and write it
      case None => for {
        stats <- forceGet(
          champions = champions,
          patches = patches,
          lastFivePatches = lastFivePatches,
          champion = champion,
          tiers = tiers,
          region = region,
          role = role,
          enemy = enemy,
          minPlayRate = minPlayRate,
          forceRefresh = forceRefresh
        )
        // TODO(igm): make this time configurable.
        result <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats

      // If the key is found, we shall parse it
      case Some(bytes) => Future.successful(MatchAggregate.parseFrom(bytes.toArray[Byte]))
    }
  }

  /**
    * Fetches and aggregates information about a single champion.
    *
    * @param patches -- The patches in the patch range we are considering.
    * @param lastFivePatches -- The last five patches of the game.
    */
  private def forceGet(
    champions: Set[Int],
    patches: Set[String],
    lastFivePatches: Seq[String],
    champion: Int,
    tiers: Set[Int],
    region: Region,
    role: Role,
    enemy: Int,
    minPlayRate: Double,
    forceRefresh: Boolean
  ): Future[MatchAggregate] = {
    // First, let's retrieve all stats for this combination.
    val allStatsFuts = patches.map { patch =>
      statistics.getSingle(
        champions, tiers, patch, region, role, enemy, forceRefresh
      ).map((patch, _))
    }

    // This future contains an element of the form Map[String, ChampionStatistics]
    // where key is the patch and value is the stats.
    val allStatsFut = Future.sequence(allStatsFuts).map(_.toMap)

    // Next, let's get per-role sums.
    val byRoleFilters = Role.values.map { someRole =>
      (someRole, MatchFilterSet(champion, patches, tiers, region, enemy, someRole).toFilterSet)
    }.toMap
    val byRoleFut = FutureUtil.sequenceMap(byRoleFilters.mapValues(filters => db.matchSums.sum(filters)))

    // Next, let's get per-patch sums.
    val byPatchFilters = lastFivePatches.map { patch =>
      (patch, MatchFilterSet(champion, patch, tiers, region, enemy, role).toFilterSet)
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
