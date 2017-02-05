package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }

import asuna.lucinda.filters.MatchFilterSet
import asuna.lucinda.matches.AggregationContext
import asuna.proto.league.{ QueueType, Region, Role, Tier }
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc.GetSumRequest
import cats.implicits._
import redis.RedisClient

case class StatisticsId(
  // TODO(igm): don't cache based on minPlayRate -- calculate on the fly
  patches: Seq[String],
  lastFivePatches: Seq[String],
  champions: Seq[Int],
  tiers: Seq[Tier],
  regions: Seq[Region],
  roles: Seq[Role],
  enemies: Seq[Int],
  queues: Seq[QueueType],
  minPlayRate: Double
)

object StatisticsId {

  /**
    * Since Seq has a deterministic print order, we convert Sets to Seqs.
    */
  def fromSets(
    patches: Set[String],
    lastFivePatches: Seq[String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[QueueType],
    minPlayRate: Double
  ): StatisticsId = StatisticsId(
    patches = patches.toSeq.sorted,
    lastFivePatches = lastFivePatches,
    champions = champions.toSeq,
    tiers = tiers.toSeq.sortBy(_.value),
    regions = regions.toSeq,
    roles = roles.toSeq,
    enemies = enemies.toSeq,
    queues = queues.toSeq.sortBy(_.value),
    minPlayRate = minPlayRate
  )

}

class StatisticsDAO(alexandria: Alexandria, redis: RedisClient, statistics: AllChampionStatisticsDAO)(
  implicit ec: ExecutionContext
) {

  def get(
    allChampions: Set[Int],
    patches: Set[String],
    lastFivePatches: Seq[String],
    prevPatches: Map[String, String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[QueueType],
    enemies: Set[Int],
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[Statistics] = {
    import scala.concurrent.duration._

    val id = StatisticsId.fromSets(
      patches = patches,
      lastFivePatches = lastFivePatches,
      champion = champion,
      tiers = tiers,
      regions = regions,
      roles = roles,
      enemies = enemies,
      queues = queues,
      minPlayRate = minPlayRate
    )
    val key = id.toString

    val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

    redisResult flatMap {
      // If the key is not found or we're using force refresh, recalculate it and write it
      case None => for {
        stats <- forceGet(
          allChampions = allChampions,
          patches = patches,
          lastFivePatches = lastFivePatches,
          prevPatches = prevPatches,
          champion = champion,
          tiers = tiers,
          regions = regions,
          roles = roles,
          enemies = enemies,
          queues = queues,
          minPlayRate = minPlayRate,
          forceRefresh = forceRefresh
        )
        // TODO(igm): make this time configurable.
        result <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats

      // If the key is found, we shall parse it
      case Some(bytes) => Future.successful(Statistics.parseFrom(bytes.toArray[Byte]))
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
    queues: Set[QueueType],
    minPlayRate: Double,
    forceRefresh: Boolean
  ): Future[Statistics] = {
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

      // This contains an element of the form Map[String, AllChampionStatistics]
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
