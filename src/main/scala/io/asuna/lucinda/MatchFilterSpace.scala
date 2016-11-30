package io.asuna.lucinda

import io.asuna.proto.enums.{ QueueType, Region, Role }
import io.asuna.proto.match_filters.MatchFilters

// TODO(igm): find a better place for this to live

/**
  * A space (set) of match filters. This is a 7-dimensional projection of
  * the space of all MatchSums.
  */
case class MatchFilterSpace(
  champion: Set[Int],
  patch: Set[String],
  tiers: Set[Int],
  region: Set[Region],
  role: Set[Role],
  queues: Set[QueueType],
  enemy: Set[Int]
) {

  /**
    * Converts this into something our database can understand.
    */
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

  /**
    * Inverts enemy and champion. This is useful for computing matchups.
    */
  def inverse = {
    copy(champion = enemy, enemy = champion)
  }

  def keyify = {
    // TODO(pradyuman): make a surefire, human readable key for this
    toString
  }

}

object MatchFilterSpace {

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
  ): MatchFilterSpace = MatchFilterSpace(
    champions = Set(champion),
    patches = Set(patch),
    tiers = tiers,
    region = Set(region),
    role = Set(role),
    queues = queues,
    enemy = Set(enemy)
  )

  /**
    * Creates a map of champion id -> MatchFilterSpace.
    */
  def championsMap(
    champions: Set[Int],
    patch: String,
    tiers: Set[Int],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    enemy: Int
  ) = for {
    champion <- champions
  } yield MatchFilterSpace(
    champion = champion,
    patch = patch,
    tiers = tiers,
    region = region,
    role = role,
    queues = queues,
    enemy = enemy
  )

}
