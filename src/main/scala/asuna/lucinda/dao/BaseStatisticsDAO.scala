package asuna.lucinda.dao

import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league._
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistics, StatisticsKey }
import monix.eval.Task
import cats.implicits._
import monix.cats._
import asuna.lucinda.util.TaskHelpers._

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
    val byRoleTask = in.base.byRoleFilters.traverseG { subspace =>
      sumFetcher.fetchSums(in, subspace)
    }

    // Patch information
    val byPatchTask = in.base.byPatchFilters.traverseG { subspace =>
      sumFetcher.fetchSums(in, subspace)
    }

    // Stats (where Statistic objects come from)
    val allStatsTask = fetchACS(in)

    // TODO(igm): reuse prev call data
    // This contains an element of the form Map[String, AllChampionStatistics]
    // where key is the patch and value is the stats.
    val patchNbhdTask = in.base.patchNbhdMap.traverseG { patch =>
      fetchPatchACS(in, patch)
    }

    List(byRoleTask, byPatchTask, allStatsTask, patchNbhdTask).sequenceG.map {
      case List(byRole, byPatch, allStats, patchNbhd) => {
        StatisticsGenerator.makeStatistics(
          champions = in.base.champions,
          allStats = allStats.asInstanceOf[AllChampionStatistics],
          patchNbhd = patchNbhd.asInstanceOf[Map[String, AllChampionStatistics]],
          roles = in.base.roles,
          byRole = byRole.asInstanceOf[Map[Role, MatchSum]],
          byPatch = byPatch.asInstanceOf[Map[String, MatchSum]],
          patches = in.base.patches
        )
      }
    }
  }

}
