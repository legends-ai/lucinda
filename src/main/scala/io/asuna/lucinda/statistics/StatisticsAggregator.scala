package io.asuna.lucinda.statistics

import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics._
import io.asuna.lucinda.database.LucindaDatabase

object StatisticsAggregator {

  def aggregate(
    patches: Set[String], tiers: Set[Int], region: Region, enemy: Int = -1
  )(implicit db: LucindaDatabase): ChampionStatistics = {
    return ChampionStatistics()
  }

  def buildFilterMap(
    patches: Set[String], tiers: Set[Int], region: Region, enemy: Int = -1
  ): Map[Int, Set[MatchFilters]] = {
    Map()
  }

  /**
    * Make the Statistics object from a role and sums for that role. This function is pure.
    */
  def makeStatistics(role: Role, rawSums: Map[Int, MatchSum]): Statistics = {
    val sums = SumCombiner.combineSums(rawSums)
    val quotients = QuotientsGenerator.generateQuotients(sums)
    val results = ResultsGenerator.generateResults(quotients)
    Statistics(
      role = role,
      results = Some(results),
      quotients = Some(quotients),
      sums = Some(sums)
    )
  }

}
