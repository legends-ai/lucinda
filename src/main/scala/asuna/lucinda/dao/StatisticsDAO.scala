package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }

import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.lucinda.matches.MinPlayRateDecorator
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
  queues: Seq[QueueType]
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
    queues: Set[QueueType]
  ): StatisticsId = StatisticsId(
    patches = patches.toSeq.sorted,
    lastFivePatches = lastFivePatches,
    champions = champions.toSeq,
    tiers = tiers.toSeq.sortBy(_.value),
    regions = regions.toSeq.sortBy(_.value),
    roles = roles.toSeq.sortBy(_.value),
    enemies = enemies.toSeq.sorted,
    queues = queues.toSeq.sortBy(_.value)
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
      queues = queues
    )
    val key = id.toString

    val redisResult = if (forceRefresh) Future.successful(None) else redis.get(key)

    val statsFut = redisResult flatMap {
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
          forceRefresh = forceRefresh
        )
        // TODO(igm): make this time configurable.
        result <- redis.set(key, stats.toByteArray, exSeconds = Some((15 minutes) toSeconds))
      } yield stats

      // If the key is found, we shall parse it
      case Some(bytes) => Future.successful(Statistics.parseFrom(bytes.toArray[Byte]))
    }
    statsFut.map { stats =>
      MinPlayRateDecorator.decorate(minPlayRate, stats)
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
    forceRefresh: Boolean
  ): Future[Statistics] = {
    // First, let's get per-role sums.
    val byRoleFilters = (Role.values.toSet - Role.UNDEFINED_ROLE).map { someRole =>
      (someRole, MatchFilterSet(champions, patches, tiers, regions, enemies, Set(someRole), queues).toFilterSet)
    }.toMap
    val byRoleFuts = byRoleFilters
      .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq)))

    for {
      byRole <- byRoleFuts.sequence

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      // TODO(igm): when casssandra perf is better, use lastFivePatches. We need this to calculate historical perf.
      // We can also calculate a matchquotient wholly derived from a MatchSum. Figure out how important this feature is.
      // allStatsPatches = patches union lastFivePatches.toSet
      allStatsPatches = patches
      allStatsFuts = allStatsPatches.toList.map { patch =>
        allChampionStatisticsDAO.getSingle(
          allChampions, tiers, Set(patch), prevPatches.get(patch), regions, roles, queues, enemies, forceRefresh
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      allStats <- allStatsFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = lastFivePatches.map { patch =>
        (patch, MatchFilterSet(champions, Set(patch), tiers, regions, enemies, roles, queues).toFilterSet)
      }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters
        .mapValues(filters => alexandria.getSum(GetSumRequest(filters = filters.toSeq))).sequence

    } yield StatisticsGenerator.makeStatistics(
      champions = champions,
      allStats = allStats,
      roles = roles,
      byRole = byRole,
      byPatch = byPatch,
      patches = patches
    )
  }

}
