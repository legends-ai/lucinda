package asuna.lucinda.filters

import cats.implicits._
import asuna.proto.league.{
  MatchFilters, QueueType, Region, Role, Tier
}

case class MatchFilterSet(
  champions: Set[Int],
  versions: Set[String],
  tiers: Set[Tier],
  regions: Set[Region],
  enemies: Set[Int],
  roles: Set[Role],
  queues: Set[QueueType]
) {

  def toFilterSet: Set[MatchFilters] = {
    // Apply usual (non-empty) filters
    val baseFilters = for {
      version <- versions
      tier <- if (tiers.size == 0) Tier.values.toSet else tiers
      region <- regions
      queue <- queues
      role <- if (roles.size == 0) (Role.values.toSet - Role.UNDEFINED_ROLE) else roles
    } yield MatchFilters(
      version = version,
      tier = tier,
      region = region,
      queue = queue,
      role = role
    )

    // Apply champ filters
    val champFilters = if (champions.size > 0) {
      baseFilters.flatMap(
        filters => champions.map(c =>
          filters.copy(championId = c.some)))
    } else {
      baseFilters
    }

    // Apply enemy filters
    if (enemies.size > 0) {
      champFilters.flatMap(
        filters => enemies.map(c =>
          filters.copy(enemyId = c.some)))
    } else {
      champFilters
    }
  }.toSet

  def inverse = {
    copy(champions = enemies, enemies = champions)
  }

}
