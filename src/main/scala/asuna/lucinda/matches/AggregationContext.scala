package asuna.lucinda.matches

import asuna.proto.enums.Role
import asuna.proto.ids.ChampionId
import asuna.proto.lucinda.LucindaData.ChampionStatistics
import asuna.proto.match_sum.MatchSum


/**
  * Context for MatchAggregate aggregation.
  */
case class AggregationContext(
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

object AggregationContext {

  def apply(champion: Option[ChampionId], minPlayRate: Double): AggregationContext = {
    AggregationContext(champion.map(_.value).getOrElse(-1), minPlayRate)
  }

}
