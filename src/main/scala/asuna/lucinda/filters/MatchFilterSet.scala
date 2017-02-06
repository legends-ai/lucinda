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
    for {
      champion <- champions
      version <- versions
      tier <- tiers
      region <- regions
      enemy <- enemies
      role <- roles
      queue <- queues
    } yield MatchFilters(
      championId = champion.some,
      version = version,
      tier = tier,
      region = region,
      enemyId = enemy.some,
      role = role,
      queue = queue
    )
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
