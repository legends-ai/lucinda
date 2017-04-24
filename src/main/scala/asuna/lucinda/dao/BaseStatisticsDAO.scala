package asuna.lucinda.dao

import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league._
import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistics, StatisticsKey }
import monix.eval.Task
import cats.implicits._
import monix.cats._
import asuna.common.monix.TaskHelpers._
import asuna.proto.league.lucinda.rpc.Constraints

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
    queues: Set[Queue],

    constraints: Constraints,
  ) {

    lazy val space = MatchFiltersSpace(
      championIds = champions.toSeq,
      versions = patches.toSeq,
      tiers = tiers.toSeq,
      regions = regions.toSeq,
      enemyIds = enemies.toSeq,
      roles = roles.toSeq,
      queues = queues.toSeq,
    )

    lazy val byRoleFilters: Map[Role, MatchFiltersSpace] = Role.values
      .map(r => (r, r)).toMap
      .mapValues { someRole =>
        space.copy(roles = Set(someRole).toSeq)
      }

    lazy val byPatchFilters: Map[String, MatchFiltersSpace] = patchNeighborhood
      .map(p => (p, p)).toMap
      .mapValues { patch =>
        space.copy(versions = Set(patch).toSeq)
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

  def fetchACS(in: K, roles: Set[Role]): Task[AllChampionStatistics]

  def fetchPatchACS(in: K, roles: Set[Role], patch: String): Task[AllChampionStatistics]

  def compute(in: K): Task[Statistics] = {
    for {
      byRole <- in.base.byRoleFilters.traverseG { subspace =>
        sumFetcher.fetchSums(in, subspace)
      }

      roles = if (in.base.roles.size === 0) {
        // The role with the most plays.
        val maxRole = byRole.toSeq
          .sortBy(_._2.statistics.map(_.plays).orEmpty)
          .lastOption
          .map(_._1)
          .getOrElse(Role.UNDEFINED_ROLE)
        Set(maxRole)
      } else in.base.roles

      // Map from patch ot matchsum
      byPatch <- in.base.byPatchFilters.traverseG { subspace =>
        sumFetcher.fetchSums(in, subspace)
      }

      // STats (where statistic objects come from)
      allStats <- fetchACS(in, roles)

      // TODO(igm): reuse prev call data
      // Map of patch to all champion statistics for a role
      patchNbhd <- in.base.patchNbhdMap.traverseG { patch =>
        fetchPatchACS(in, roles, patch)
      }

      // TODO(igm): parallelize
    } yield StatisticsGenerator.makeStatistics(
      champions = in.base.champions,
      allStats = allStats,
      patchNbhd = patchNbhd,
      roles = roles,
      byRole = byRole,
      byPatch = byPatch,
      patches = in.base.patches,
    )
  }

}
