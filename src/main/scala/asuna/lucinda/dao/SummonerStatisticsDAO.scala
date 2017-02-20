package asuna.lucinda.dao

import asuna.common.legends.MatchSumHelpers._
import asuna.lucinda.matches.StatisticsGenerator
import cats.implicits._
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.{ MatchFiltersSpace, SummonerId }
import asuna.proto.league.alexandria.rpc.GetSummonerMatchSumRequest
import asuna.proto.league.{ Queue, Region, Role, Tier }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.lucinda.Statistics
import scala.concurrent.{ ExecutionContext, Future }

class SummonerStatisticsDAO(
  alexandria: Alexandria, summonerChampionsDAO: SummonerChampionsDAO
)(implicit ec: ExecutionContext) {

  def get(
    id: SummonerId,
    allChampions: Set[Int],
    lastFivePatches: Seq[String],
    prevPatches: Map[String, String],

    champions: Set[Int],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyIds: Set[Int]
  ): Future[Statistics] = {
    val space = MatchFilterSpaceHelpers.generate(
      champions, patches, Set(Tier.UNRANKED), Set(id.region), enemyIds, roles, queues)

    // First, let's get per-role sums.
    val byRoleFilters = (Role.values.toSet - Role.UNDEFINED_ROLE)
      .map(r => (r, r)).toMap
      .mapValues { someRole =>
        space.copy(roles = Seq(someRole))
      }
    val byRoleFuts = byRoleFilters.mapValues { subspace =>
      val req = GetSummonerMatchSumRequest(
        summoner = id.some,
        space = subspace.some
      )
      alexandria.getSummonerMatchSum(req)
    }

    for {
      byRole <- byRoleFuts.sequence

      // Next, let's retrieve all stats for this combination.
      // This is used to get Statistic objects.
      allStatsPatches = patches union lastFivePatches.toSet
      allStatsFuts = allStatsPatches.toList.map { patch =>
        summonerChampionsDAO.get(
          id = id,
          allChampions = allChampions,
          prevPatch = prevPatches.get(patch),
          roles = roles,
          patches = Set(patch),
          queues = queues,
          enemyIds = enemyIds
        ).map((patch, _))
      }

      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      allStats <- allStatsFuts.sequence.map(_.toMap)

      // Finally, let's get the patch information.
      // We'll use a map with the key being the patch.
      byPatchFilters = lastFivePatches
        .map(p => (p, p)).toMap
        .mapValues { patch =>
          space.copy(versions = Seq(patch))
        }.toMap

      // We will then sequence them.
      byPatch <- byPatchFilters.mapValues { subspace =>
        val req = GetSummonerMatchSumRequest(
          summoner = id.some,
          space = subspace.some
        )
        alexandria.getSummonerMatchSum(req)
      }.sequence

    } yield StatisticsGenerator.makeStatistics(
      champions = champions,
      allStats = allStats,
      roles = roles,
      byRole = byRole.mapValues(_.matchSum.orEmpty),
      byPatch = byPatch.mapValues(_.matchSum.orEmpty),
      patches = patches
    )
  }

}
