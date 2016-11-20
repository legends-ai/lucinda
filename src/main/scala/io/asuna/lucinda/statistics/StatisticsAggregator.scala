package io.asuna.lucinda.statistics

import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.match_filters.MatchFilters

object StatisticsAggregator {

  def buildFilterSet(
    championId: Int, patch: String, tiers: Set[Int],
    region: Region, role: Role, enemy: Int = -1
  ): Set[MatchFilters] = {
    for {
      tier <- tiers
    } yield MatchFilters(
      championId = championId,
      patch = patch,
      tier = tier,
      region = region,
      enemyId = enemy,
      role = role
    )
  }

  /**
    * Make the Statistics object from a role and sums for that role. This function is p u r e.
    */
  def makeStatistics(role: Role, rawSums: Map[Int, MatchSum]): ChampionStatistics = {
    val sums = SumCombiner.combineSums(rawSums)
    val quotients = QuotientsGenerator.generateQuotients(sums)
    val results = ResultsGenerator.generateResults(quotients)
    ChampionStatistics(
      role = role,
      results = Some(results),
      quotients = Some(quotients),
      sums = Some(sums)
    )
  }

}
