package asuna.lucinda.statistics

import asuna.proto.enums.{Region, Role}
import asuna.proto.ids.ChampionId
import asuna.proto.lucinda.LucindaData._
import asuna.proto.match_sum.MatchSum
import asuna.proto.match_filters.MatchFilters

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
