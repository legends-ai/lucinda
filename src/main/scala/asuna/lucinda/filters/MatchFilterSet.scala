package asuna.lucinda.filters

import asuna.proto.league.{
  MatchFilters, QueueType, Region, Role, Tier
}

case class MatchFilterSet(
  champion: Option[Int],
  versions: Set[String],
  tiers: Set[Tier],
  region: Region,
  enemy: Option[Int],
  role: Role,
  queues: Set[QueueType]
) {

  def toFilterSet: Set[MatchFilters] = {
    for {
      version <- versions
      tier <- tiers
      queue <- queues
    } yield MatchFilters(
      championId = champion,
      version = version,
      tier = tier,
      region = region,
      enemyId = enemy,
      role = role,
      queue = queue
    )
  }.toSet

  def inverse = {
    copy(champion = enemy, enemy = champion)
  }

}

object MatchFilterSet {

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
