package asuna.lucinda.statistics

import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda._

object StatisticsAggregator {

  /**
    * Make the Statistics object from a role and sums for that role. This function is p u r e.
    */
  def makeStatistics(roleCount: Int, rawSums: Map[Int, MatchSum]): AllChampionStatistics = {
    val sums = SumCombiner.combineSums(rawSums)
    val quotients = QuotientsGenerator.generateQuotients(sums)
    val results = ResultsGenerator.generate(roleCount, sums, quotients)
    AllChampionStatistics(
      results = Some(results),
      quotients = Some(quotients),
      sums = Some(sums),
      roleCount = roleCount,
    )
  }

}
