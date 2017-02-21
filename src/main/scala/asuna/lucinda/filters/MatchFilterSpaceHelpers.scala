package asuna.lucinda.filters

import cats.implicits._
import asuna.proto.league.{
  MatchFiltersSpace, Queue, Region, Role, Tier
}

object MatchFilterSpaceHelpers {

  def generate(
    championIds: Set[Int],
    versions: Set[String],
    tiers: Set[Tier],
    regions: Set[Region],
    enemyIds: Set[Int],
    roles: Set[Role],
    queues: Set[Queue]
  ): MatchFiltersSpace = MatchFiltersSpace(
    championIds = championIds.toSeq,
    versions = versions.toSeq,
    tiers = tiers.toSeq,
    regions = regions.toSeq,
    enemyIds = enemyIds.toSeq,
    roles = roles.toSeq,
    queues = queues.toSeq
  )

}
