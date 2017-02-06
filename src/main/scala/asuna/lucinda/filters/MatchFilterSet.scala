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
      tier <- tiers
      region <- regions
      role <- roles
      queue <- queues
    } yield MatchFilters(
      version = version,
      tier = tier,
      region = region,
      role = role,
      queue = queue
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

object MatchFilterSet {

  def apply(
    champion: Option[Int],
    versions: Set[String],
    tiers: Set[Tier],
    region: Region,
    enemy: Option[Int],
    role: Role,
    queues: Set[QueueType]
  ): MatchFilterSet = MatchFilterSet(
    champions = champion.map(Set(_)).getOrElse(Set()),
    versions = versions,
    tiers = tiers,
    regions = Set(region),
    enemies = enemy.map(Set(_)).getOrElse(Set()),
    roles = Set(role),
    queues = queues
  )

  def apply(
    champion: Option[Int],
    version: String,
    tiers: Set[Tier],
    region: Region,
    enemy: Option[Int],
    role: Role,
    queues: Set[QueueType]
  ): MatchFilterSet = MatchFilterSet(
    champion = champion,
    versions = Set(version),
    tiers = tiers,
    region = region,
    enemy = enemy,
    role = role,
    queues = queues
  )

}
