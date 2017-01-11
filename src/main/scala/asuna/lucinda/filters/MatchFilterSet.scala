package asuna.lucinda.filters

import asuna.proto.ids.ChampionId
import asuna.proto.match_filters.MatchFilters
import asuna.proto.enums.{ QueueType, Region, Role }
import asuna.proto.enums.Tier

case class MatchFilterSet(
  champion: Option[ChampionId],
  patches: Set[String],
  tiers: Set[Tier],
  region: Region,
  enemy: Option[ChampionId],
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

  def apply(
    champion: Option[ChampionId],
    patch: String,
    tiers: Set[Tier],
    region: Region,
    enemy: Option[ChampionId],
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
