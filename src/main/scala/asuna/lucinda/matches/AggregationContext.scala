package asuna.lucinda.matches

import asuna.proto.league.{ MatchSum, Role }
import asuna.proto.league.lucinda.AllChampionStatistics

/**
  * Context for MatchAggregate aggregation.
  */
case class AggregationContext(
  champion: Int,
  minPlayRate: Double
) {

  def aggregate(
    patchStats: Map[String, AllChampionStatistics],
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

  def apply(champion: Option[Int], minPlayRate: Double): AggregationContext = {
    AggregationContext(champion.map(_.value).getOrElse(-1), minPlayRate)
  }

}
