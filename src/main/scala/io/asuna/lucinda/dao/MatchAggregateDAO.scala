package io.asuna.lucinda.dao

import io.asuna.lucinda.filters.MatchFilterSet
import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.lucinda.matches.AggregationContext
import io.asuna.proto.enums.{ QueueType, Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion.MatchAggregate
import redis.RedisClient
import scala.concurrent.{ ExecutionContext, Future }
import cats.implicits._

case class MatchAggregateId(
  // TODO(igm): don't cache based on minPlayRate -- calculate on the fly
  patches: List[String],
  lastFivePatches: List[String],
  champion: Int,
  tiers: List[Int],
  region: Region,
  role: Role,
  enemy: Int,
  queues: List[QueueType],
  minPlayRate: Double
)

object MatchAggregateId {

  /**
    * Since List has a deterministic print order, we convert Sets to Lists.
    */
  def fromSets(
    patches: Set[String],
    lastFivePatches: List[String],
    champion: Int,
    tiers: Set[Int],
    region: Region,
    role: Role,
    enemy: Int,
    queues: Set[QueueType],
    minPlayRate: Double
  ): MatchAggregateId = MatchAggregateId(
    patches = patches.toList.sorted,
    lastFivePatches = lastFivePatches,
    champion = champion,
    tiers = tiers.toList.sorted,
    region = region,
    role = role,
    enemy = enemy,
    queues = queues.toList.sortBy(_.value),
    minPlayRate = minPlayRate
  )

}

class MatchAggregateDAO(db: LucindaDatabase, redis: RedisClient, statistics: ChampionStatisticsDAO)(implicit ec: ExecutionContext) {

  def get(
    champions: Set[Int],
    patches: Set[String],
    lastFivePatches: List[String],
    champion: Int,
    tiers: Set[Int],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int = -1,
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[MatchAggregate] = {
    import scala.concurrent.duration._

    val id = MatchAggregateId.fromSets(
      patches = patches,
      lastFivePatches = lastFivePatches,
      champion = champion,
      tiers = tiers,
      region = region,
      role = role,
      enemy = enemy,
      queues = queues,
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
          queues = queues,
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
    queues: Set[QueueType],
    minPlayRate: Double,
    forceRefresh: Boolean
  ): Future[MatchAggregate] = {
    // First, let's retrieve all stats for this combination.
    val allStatsFuts = patches.toList.map { patch =>
      statistics.getSingle(
        champions, tiers, patch, region, role, queues, enemy, forceRefresh
      ).map((patch, _))
    }

    // This future contains an element of the form Map[String, ChampionStatistics]
    // where key is the patch and value is the stats.
    val allStatsFut = allStatsFuts.sequence.map(_.toMap)

    // Next, let's get per-role sums.
    val byRoleFilters = Role.values.map { someRole =>
      (someRole, MatchFilterSet(champion, patches, tiers, region, enemy, someRole).toFilterSet)
    }.toMap
    val byRoleFut = byRoleFilters.mapValues(filters => db.matchSums.sum(filters)).sequence

    // Next, let's get per-patch sums.
    val byPatchFilters = lastFivePatches.map { patch =>
      (patch, MatchFilterSet(champion, patch, tiers, region, enemy, role).toFilterSet)
    }.toMap
    val byPatchFut = byPatchFilters.mapValues(filters => db.matchSums.sum(filters)).sequence

    val ctx = AggregationContext(champion, minPlayRate)

    // Finally, we'll execute and build everything.
    (allStatsFut |@| byRoleFut |@| byPatchFut).map(ctx.aggregate)
  }

}
