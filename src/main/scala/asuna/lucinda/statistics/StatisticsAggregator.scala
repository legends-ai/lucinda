package asuna.lucinda.statistics

import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda._

object StatisticsAggregator {

  /**
    * Make the Statistics object from a role and sums for that role. This function is p u r e.
    */
  def makeStatistics(
    rawSums: Map[Int, MatchSum],
    bans: Map[Int, Int],
    picks: Seq[AllChampionStatistics.Sums.Subscalars.PickStats],
  ): AllChampionStatistics = {
    val initSums = SumCombiner.combineSums(rawSums)
    val sums = initSums.update(
      _.subscalars.bans := bans,
      _.subscalars.picks := picks,
    )
    val results = ResultsGenerator.generate(sums)
    AllChampionStatistics(
      results = Some(results),
      sums = Some(sums),
    )
  }

}
