package asuna.lucinda.dao

import asuna.proto.league._
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.proto.league.lucinda.StatisticsKey

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

    lazy val alexandriaKey: StatisticsKey = StatisticsKey(
      championIds = champions.toSeq,
      patches = patches.toSeq,
      tiers = tiers.toSeq,
      regions = regions.toSeq,
      roles = roles.toSeq,
      enemyIds = enemies.toSeq,
      queues = queues.toSeq
    )

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

}

trait BaseStatisticsDAO {

}
