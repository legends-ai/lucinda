package asuna.lucinda.dao

import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.alexandria.StoredStatistics
import asuna.proto.league.alexandria.rpc.UpsertStatisticsRequest
import asuna.proto.league.lucinda.StatisticsKey
import scala.concurrent.{ ExecutionContext, Future }

import asuna.lucinda.matches.MinPickRateDecorator
import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league.{ Queue, Region, Role, Tier }
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc.GetSumRequest
import cats.implicits._

class StatisticsDAO(
  alexandria: Alexandria, allChampionStatisticsDAO: AllChampionStatisticsDAO
)(implicit ec: ExecutionContext) {

  def get(
    allChampions: Set[Int],
    patches: Set[String],
    patchNeighborhood: Seq[String],
    prevPatch: Option[String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    minPickRate: Double
  ): Future[Statistics] = {
    val key = keyFromSets(
      patches = patches,
      champions = champions,
      tiers = tiers,
      regions = regions,
      roles = roles,
      enemies = enemies,
      queues = queues
    )

    // Fetch champ statistics from the cache
    val statsFut = alexandria.getStatistics(key) flatMap {
      // If the key is found, we shall parse it
      // TODO(igm): if TS time remaining is low enough, refetch
      case StoredStatistics(Some(data), _) => Future.successful(data)

      // If the key is not found or we're using force refresh, recalculate it and write it
      case _ => for {
        stats <- forceGet(
          allChampions = allChampions,
          patches = patches,
          patchNeighborhood = patchNeighborhood,
          prevPatch = prevPatch,
          champions = champions,
          tiers = tiers,
          regions = regions,
          roles = roles,
          enemies = enemies,
          queues = queues
        )

        // Insert back to alexandria
        req = UpsertStatisticsRequest(
          key = key.some,
          value = stats.some
        )
        _ <- alexandria.upsertStatistics(req)
      } yield stats
    }

    statsFut.map { stats =>
      MinPickRateDecorator.decorate(minPickRate, stats)
    }
  }

  /**
    * Fetches and aggregates information about a single champion.
    *
    * @param patches -- The patches in the patch range we are considering.
    * @param patchNeighborhood -- The patch neighborhood of the patch range.
    */
  private def forceGet(
    allChampions: Set[Int],
    patches: Set[String],
    patchNeighborhood: Seq[String],
    prevPatch: Option[String],

    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue]
  ): Future[Statistics] = {
    val space = MatchFilterSpaceHelpers.generate(
      champions, patches, tiers, regions, enemies, roles, queues)

    // First, let's get per-role sums.
    val byRoleFilters = (Role.values.toSet - Role.UNDEFINED_ROLE)
      .map(r => (r, r)).toMap
      .mapValues { someRole =>
        space.copy(roles = Seq(someRole))
      }
    val byRoleFuts = byRoleFilters
      .mapValues(subspace => alexandria.getSum(GetSumRequest(space = subspace.some)))

    for {
      byRole <- byRoleFuts.sequence

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      allStats <- allChampionStatisticsDAO.get(
        allChampions, tiers, patches, prevPatch,
        regions, roles, queues, enemies
      )

      // TODO(igm): reuse prev call data
      patchNbhdFuts = patchNeighborhood.toList.map { patch =>
        allChampionStatisticsDAO.get(
          allChampions, tiers, Set(patch), None,
          regions, roles, queues, enemies
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      patchNbhd <- patchNbhdFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = patchNeighborhood
        .map(p => (p, p)).toMap
        .mapValues { patch =>
          space.copy(versions = Seq(patch))
        }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters
        .mapValues(subspace => alexandria.getSum(GetSumRequest(space = subspace.some))).sequence

    } yield StatisticsGenerator.makeStatistics(
      champions = champions,
      allStats = allStats,
      patchNbhd = patchNbhd,
      roles = roles,
      byRole = byRole,
      byPatch = byPatch,
      patches = patches
    )
  }

  private def keyFromSets(
    patches: Set[String],
    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue]
  ): StatisticsKey = StatisticsKey(
    championIds = champions.toSeq,
    patches = patches.toSeq,
    tiers = tiers.toSeq,
    regions = regions.toSeq,
    roles = roles.toSeq,
    enemyIds = enemies.toSeq,
    queues = queues.toSeq
  )


}
