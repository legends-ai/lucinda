package io.asuna.lucinda

import io.asuna.proto.enums.{ QueueType, Region, Role }
import io.asuna.proto.match_filters.MatchFilters

// TODO(igm): find a better place for this to live

/**
  * A space (set) of match filters.
  */
case class MatchFilterSpace(
  champion: Int,
  patch: String,
  tiers: Set[Int],
  region: Region,
  role: Role,
  queues: Set[QueueType],
  enemy: Int
) {

  def toFilterSet: Set[MatchFilters] = for {
    tier <- tiers
    queue <- queues
  } yield MatchFilters(
    championId = champion,
    patch = patch,
    tier = tier,
    region = region,
    enemyId = enemy,
    role = role,
    queue = queue
  )

  def inverse = {
    copy(champion = enemy, enemy = champion)
  }

}

