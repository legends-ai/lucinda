package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }

import asuna.lucinda.filters.MatchFilterSet
import asuna.lucinda.matches.AggregationContext
import asuna.proto.league.{ ChampionId, QueueType, Region, Role, Tier }
import asuna.proto.league.lucinda.Champion.MatchAggregate
import asuna.proto.league.alexandria.GetSumRequest
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import cats.implicits._
import redis.RedisClient

case class MatchAggregateId(
  // TODO(igm): don't cache based on minPlayRate -- calculate on the fly
  patches: List[String],
  lastFivePatches: List[String],
  champion: Int,
  tiers: List[Tier],
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
    champion: Option[ChampionId],
    tiers: Set[Tier],
    region: Region,
    role: Role,
    enemy: Option[ChampionId],
    queues: Set[QueueType],
    minPlayRate: Double
  ): MatchAggregateId = MatchAggregateId(
    patches = patches.toList.sorted,
    lastFivePatches = lastFivePatches,
    champion = champion.map(_.value).getOrElse(-1),
    tiers = tiers.toList.sortBy(_.value),
    region = region,
    role = role,
    enemy = enemy.map(_.value).getOrElse(-1),
    queues = queues.toList.sortBy(_.value),
    minPlayRate = minPlayRate
  )

}

class MatchAggregateDAO(alexandria: Alexandria, redis: RedisClient, statistics: ChampionStatisticsDAO)(implicit ec: ExecutionContext) {

  def get(
    champions: Set[ChampionId],
    patches: Set[String],
    lastFivePatches: List[String],
    prevPatches: Map[String, String],
    champion: Option[ChampionId],
    tiers: Set[Tier],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Option[ChampionId],
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
          prevPatches = prevPatches,
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
    champions: Set[ChampionId],
    patches: Set[String],
    lastFivePatches: Seq[String],
    prevPatches: Map[String, String],
    champion: Option[ChampionId],
    tiers: Set[Tier],
    region: Region,
    role: Role,
    enemy: Option[ChampionId],
    queues: Set[QueueType],
    minPlayRate: Double,
    forceRefresh: Boolean
  ): Future[MatchAggregate] = {
    // First, let's get per-role sums.
    val byRoleFilters = (Role.values.toSet - Role.UNDEFINED_ROLE).map { someRole =>
      (someRole, MatchFilterSet(champion, patches, tiers, region, enemy, someRole, queues).toFilterSet)
    }.toMap
    val byRoleFuts = byRoleFilters
      .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq)))

    for {
      byRole <- byRoleFuts.sequence

      // Now, let's find the role we want to use to fetch other data.
      // If the Role was not specified, we'll return the most popular role.
      chosenRole = if (role == Role.UNDEFINED_ROLE) {
        // Sort the list by number of plays, getting the Role with the most plays.
        byRole.toList.sortBy(_._2.scalars.map(_.plays).orEmpty).last._1
      } else role

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      allStatsFuts = patches.toList.map { patch =>
        statistics.getSingle(
          champions, tiers, patch, prevPatches.get(patch), region, chosenRole, queues, enemy, forceRefresh
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, ChampionStatistics]
      // where key is the patch and value is the stats.
      allStats <- allStatsFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = lastFivePatches.map { patch =>
        (patch, MatchFilterSet(champion, patch, tiers, region, enemy, chosenRole, queues).toFilterSet)
      }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters
        .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq))).sequence

    } yield AggregationContext(champion, minPlayRate).aggregate(
      patchStats = allStats,
      byRole = byRole,
      byPatch = byPatch
    )
  }

}
