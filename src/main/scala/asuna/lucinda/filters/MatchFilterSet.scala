package asuna.lucinda.filters

import asuna.proto.match_filters.MatchFilters
import asuna.proto.enums.{ QueueType, Region, Role }

case class MatchFilterSet(
  champion: Int,
  patches: Set[String],
  tiers: Set[Int],
  region: Region,
  enemy: Int,
  role: Role,
  queues: Set[QueueType]
) {

  def toFilterSet: Set[MatchFilters] = {
    for {
      patch <- patches
      tier <- tiers
      queue <- queues
    } yield MatchFilters(
      championId = champion,
      patch = patch,
      tier = tier,
      region = region,
      enemyId = enemy,
      role = role
    )
  }.toSet

  def inverse = {
    copy(champion = enemy, enemy = champion)
  }

}

object MatchFilterSet {

  val defaultQueues = Set(QueueType.RANKED_FLEX_SR, QueueType.TEAM_BUILDER_DRAFT_RANKED_5x5)

  def apply(
    champion: Int,
    patches: Set[String],
    tiers: Set[Int],
    region: Region,
    enemy: Int,
    role: Role
  ): MatchFilterSet = MatchFilterSet(
    champion = champion,
    patches = patches,
    tiers = tiers,
    region = region,
    enemy = enemy,
    role = role,
    queues = defaultQueues
  )

  def apply(
    champion: Int,
    patch: String,
    tiers: Set[Int],
    region: Region,
    enemy: Int,
    role: Role
  ): MatchFilterSet = MatchFilterSet(
    champion = champion,
    patches = Set(patch),
    tiers = tiers,
    region = region,
    enemy = enemy,
    role = role,
    queues = defaultQueues
  )

  def apply(
    champion: Int,
    patch: String,
    tiers: Set[Int],
    region: Region,
    enemy: Int,
    role: Role,
    queues: Set[QueueType]
  ): MatchFilterSet = MatchFilterSet(
    champion = champion,
    patches = Set(patch),
    tiers = tiers,
    region = region,
    enemy = enemy,
    role = role,
    queues = queues
  )

}
