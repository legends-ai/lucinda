package asuna.lucinda.matches

import asuna.proto.enums.Role
import asuna.proto.lucinda.LucindaData.ChampionStatistics
import asuna.proto.match_sum.MatchSum


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
