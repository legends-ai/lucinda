package io.asuna.lucinda

import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import io.asuna.proto.enums.{ QueueType, Region, Role }
import io.asuna.proto.match_filters.MatchFilters

// TODO(igm): find a better place for this to live

/**
  * A space (set) of match filters. This is a 7-dimensional projection of
  * the space of all MatchSums.
  */
trait MatchFilterSpace {
  def champions: Set[Int]
  def patches: Set[String]
  def tiers: Set[Int]
  def regions: Set[Region]
  def roles: Set[Role]
  def queues: Set[QueueType]
  def enemies: Set[Int]

  /**
    * Converts this into something our database can understand.
    */
  def toFilterSet: Set[MatchFilters] = for {
    champion <- champions
    patch <- patches
    tier <- tiers
    region <- regions
    role <- roles
    queue <- queues
    enemy <- enemies
  } yield MatchFilters(
    championId = champion,
    patch = patch,
    tier = tier,
    region = region,
    enemyId = enemy,
    role = role,
    queue = queue
  )

  /**
    * Inverts enemy and champion. This is useful for computing matchups.
    */
  def inverse = {
    DefaultMatchFilterSpace(
      champions = enemies,
      patches = patches,
      tiers = tiers,
      regions = regions,
      roles = roles,
      queues = queues,
      enemies = champions
    )
  }

  def keyify = {
    // TODO(pradyuman): make a surefire, human readable key for this
    toString
  }

}

case class DefaultMatchFilterSpace(
  champions: Set[Int],
  patches: Set[String],
  tiers: Set[Int],
  regions: Set[Region],
  roles: Set[Role],
  queues: Set[QueueType],
  enemies: Set[Int]
) extends MatchFilterSpace

/**
  * A reduced match filter space which is fixed to a champion.
  */
case class CReducedMatchFilterSpace(
  champion: Int,
  patch: Set[String],
  tiers: Set[Int],
  region: Set[Region],
  role: Set[Role],
  queues: Set[QueueType],
  enemy: Set[Int]
) extends MatchFilterSpace {
  override def champions = Set(champion)
}

/**
  * A reduced match filter space which is fixed to a champion and patch.
  */
case class CPReducedMatchFilterSpace(
  champion: Int,
  patch: String,
  tiers: Set[Int],
  regions: Set[Region],
  roles: Set[Role],
  queues: Set[QueueType],
  enemies: Set[Int]
) extends MatchFilterSpace {
  override def champions = Set(champion)
  override def patches = Set(patch)
}

object MatchFilterSpace {

  val rankedQueues = Set(QueueType.RANKED_FLEX_SR, QueueType.TEAM_BUILDER_DRAFT_RANKED_5x5)

  def apply(
    factors: AggregationFactors,
    region: Region,
    queues: Set[QueueType],
    roles: Set[Role] = Role.values.toSet,
    enemy: Int = -1
  ): Map[Role, Map[String, Map[Int, CPReducedMatchFilterSpace]]] = {
    val rolesMap = roles.map(r => (r, r)).toMap
    rolesMap mapValues { role =>
      patchChampionsMap(
        champions = factors.champions.toSet,
        patches = factors.patches.toSet,
        tiers = factors.tiers.toSet,
        regions = Set(region),
        roles = Set(role),
        queues = queues,
        enemies = Set(enemy)
      )
    }
  }

  /**
    * The usual case.
    */
  def apply(
    champion: Int,
    patch: String,
    tiers: Set[Int],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int
  ): MatchFilterSpace = DefaultMatchFilterSpace(
    champions = Set(champion),
    patches = Set(patch),
    tiers = tiers,
    regions = Set(region),
    roles = Set(role),
    queues = queues,
    enemies = Set(enemy)
  )

  /**
    * Creates a map used for generating champion statistics.
    */
  def patchChampionsMap(
    champions: Set[Int],
    patches: Set[String],
    tiers: Set[Int],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[QueueType],
    enemies: Set[Int]
  ): Map[String, Map[Int, CPReducedMatchFilterSpace]] = {
    val patchesMap = patches.map(p => (p, p)).toMap
    patchesMap.mapValues { patch =>
      val championsMap = champions.map(c => (c, c)).toMap
      championsMap.mapValues { champion =>
        CPReducedMatchFilterSpace(
          champion = champion,
          patch = patch,
          tiers = tiers,
          regions = regions,
          roles = roles,
          queues = queues,
          enemies = enemies
        )
      }.toMap
    }.toMap
  }

}
