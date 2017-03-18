package asuna.lucinda.dao

import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league._
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistics, StatisticsKey }
import monix.eval.Task
import cats.implicits._
import monix.cats._

object BaseStatisticsDAO {

  /**
    * @param patchNeighborhood Surrounding patches to display stats about.
    */
  case class Key(
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
  ) {

    lazy val space = MatchFilterSpaceHelpers.generate(
      champions, patches, tiers, regions, enemies, roles, queues)

    lazy val byRoleFilters: Map[Role, MatchFiltersSpace] = (Role.values.toSet - Role.UNDEFINED_ROLE)
      .map(r => (r, r)).toMap
      .mapValues { someRole =>
        space.copy(roles = Seq(someRole))
      }

    lazy val byPatchFilters: Map[String, MatchFiltersSpace] = patchNeighborhood
      .map(p => (p, p)).toMap
      .mapValues { patch =>
        space.copy(versions = Seq(patch))
      }.toMap

    lazy val patchNbhdMap: Map[String, String] =
      patchNeighborhood.map(p => (p, p)).toMap
  }

  trait CompositeKey {
    def base: Key
  }

}

trait BaseStatisticsDAO[K <: BaseStatisticsDAO.CompositeKey] extends EphemeralDAO[K, Statistics] {

  val sumFetcher: SumFetcher[K]

  def fetchACS(in: K): Task[AllChampionStatistics]

  def fetchPatchACS(in: K, patch: String): Task[AllChampionStatistics]

  def compute(in: K): Task[Statistics] = {
    // Role information
    val byRoleTask = in.base.byRoleFilters.traverse { subspace =>
      sumFetcher.fetchSums(in, subspace)
    }

    // Patch information
    val byPatchTask = in.base.byPatchFilters.traverse { subspace =>
      sumFetcher.fetchSums(in, subspace)
    }

    // Stats (where Statistic objects come from)
    val allStatsTask = fetchACS(in)

    // TODO(igm): reuse prev call data
    // This contains an element of the form Map[String, AllChampionStatistics]
    // where key is the patch and value is the stats.
    val patchNbhdTask = in.base.patchNbhdMap.traverse { patch =>
      fetchPatchACS(in, patch)
    }

    (byRoleTask |@| byPatchTask |@| allStatsTask |@| patchNbhdTask) map {
      case (byRole, byPatch, allStats, patchNbhd) => {
        StatisticsGenerator.makeStatistics(
          champions = in.base.champions,
          allStats = allStats,
          patchNbhd = patchNbhd,
          roles = in.base.roles,
          byRole = byRole,
          byPatch = byPatch,
          patches = in.base.patches
        )
      }
    }
  }

}
