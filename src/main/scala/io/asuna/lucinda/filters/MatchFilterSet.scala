package io.asuna.lucinda.filters

import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.enums.{ QueueType, Region, Role }

case class MatchFilterSet(
  champion: Int,
  patch: String,
  tiers: Set[Int],
  region: Region,
  enemy: Int,
  role: Role,
  queues: Set[QueueType] = Set(QueueType.RANKED_FLEX_SR, QueueType.TEAM_BUILDER_DRAFT_RANKED_5x5)
) {

  def toFilterSet: Set[MatchFilters] = {
    for {
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
