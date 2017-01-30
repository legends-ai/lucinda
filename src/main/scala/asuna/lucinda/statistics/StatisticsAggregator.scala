package asuna.lucinda.statistics

import asuna.proto.league.{ ChampionId, MatchSum, MatchFilters, Region, Role }
import asuna.proto.league.lucinda._

object StatisticsAggregator {

  /**
    * Make the Statistics object from a role and sums for that role. This function is p u r e.
    */
  def makeStatistics(role: Role, rawSums: Map[Int, MatchSum]): ChampionStatistics = {
    val sums = SumCombiner.combineSums(rawSums)
    val quotients = QuotientsGenerator.generateQuotients(sums)
    val results = ResultsGenerator(sums, quotients).generate
    ChampionStatistics(
      role = role,
      results = Some(results),
      quotients = Some(quotients),
      sums = Some(sums)
    )
  }

}
