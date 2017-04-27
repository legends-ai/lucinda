package asuna.lucinda.statistics

import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda._
import AllChampionStatistics.Sums.Subscalars.Subscalar
import MatchSum.Collections.Subscalars

object StatisticsAggregator {

  def liftSubscalar(in: Subscalars): Subscalar = {
    Subscalar(
      plays = Map(0 -> in.plays),
      wins = Map(0 -> in.wins),
    )
  }

  /**
    * Make the Statistics object from a role and sums for that role. This function is p u r e.
    */
  def makeStatistics(
    roleCount: Int, rawSums: Map[Int, MatchSum], bans: Map[Int, Subscalars],
  ): AllChampionStatistics = {
    val initSums = SumCombiner.combineSums(rawSums)
    val sums = initSums.update(_.subscalars.bans := bans.mapValues(liftSubscalar))
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
