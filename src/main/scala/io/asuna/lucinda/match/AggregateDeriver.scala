package io.asuna.lucinda.statistics

object AggregateDeriver {

  def derive(
    role: Role, statistics: ChampionStatistics,
    roles: Map[Role, MatchSum], patches: Map[String, MatchSum],
    id: Int, minPlayRate: Double
  ): MatchAggregate = {
  }

}
