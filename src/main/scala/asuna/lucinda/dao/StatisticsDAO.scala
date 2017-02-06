package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }

import asuna.lucinda.filters.MatchFilterSet
import asuna.lucinda.matches.StatisticsGenerator
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
    regions = regions.toSeq.sortBy(_.value),
    roles = roles.toSeq.sortBy(_.value),
    enemies = enemies.toSeq.sorted,
    queues = queues.toSeq.sortBy(_.value),
    minPlayRate = minPlayRate
  )

}

class StatisticsDAO(
  alexandria: Alexandria, redis: RedisClient, allChampionStatisticsDAO: AllChampionStatisticsDAO
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
    queues: Set[QueueType],
    enemies: Set[Int],
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[Statistics] = {
    import scala.concurrent.duration._

    val id = StatisticsId.fromSets(
      patches = patches,
      lastFivePatches = lastFivePatches,
      champions = champions,
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
          champions = champions,
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
      (someRole, MatchFilterSet(champions, patches, tiers, regions, enemies, roles, queues).toFilterSet)
    }.toMap
    val byRoleFuts = byRoleFilters
      .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq)))

    for {
      byRole <- byRoleFuts.sequence

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      allStatsFuts = patches.toList.map { patch =>
        allChampionStatisticsDAO.getSingle(
          champions, tiers, patches, prevPatches.get(patch), regions, roles, queues, enemies, forceRefresh
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      allStats <- allStatsFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = lastFivePatches.map { patch =>
        (patch, MatchFilterSet(champions, patches, tiers, regions, enemies, roles, queues).toFilterSet)
      }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters
        .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq))).sequence

    } yield StatisticsGenerator.makeStatistics(champions, minPlayRate, allStats, roles, byRole, byPatch)
  }

}
