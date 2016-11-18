package io.asuna.lucinda.statistics

import io.asuna.lucinda.FutureUtil
import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics._
import io.asuna.lucinda.database.LucindaDatabase
import scala.concurrent.{ ExecutionContext, Future }

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
