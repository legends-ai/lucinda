package io.asuna.lucinda.matches

import io.asuna.proto.enums.Role
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics
import io.asuna.proto.match_sum.MatchSum


/**
  * Context for MatchAggregate aggregation.
  */
case class AggregationContext (
  champion: Int,
  minPlayRate: Double
) {

  def aggregate(
    patchStats: Map[String, ChampionStatistics],
    byRole: Map[Role, MatchSum],
    byPatch: Map[String, MatchSum]
  ) = {
    MatchAggregator.makeAggregate(
      champion, minPlayRate,
      patchStats, byRole, byPatch
    )
  }

}
