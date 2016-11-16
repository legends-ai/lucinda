package io.asuna.lucinda.statistics

import io.asuna.proto.enums.Role
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics

object StatisticsCombiner {

  def combineMulti(aList: Iterable[ChampionStatistics]): ChampionStatistics = {
    if (aList.size == 0) {
      return ChampionStatistics()
    }
    combine(aList.head, combineMulti(aList.tail))
  }

  def combine(a: ChampionStatistics, b: ChampionStatistics): ChampionStatistics = {
    val aRole = a.statistics.groupBy(_.role).mapValues(_.head)
    val bRole = b.statistics.groupBy(_.role).mapValues(_.head)
    val combinedList = for {
      role <- aRole.keys ++ bRole.keys
    } yield combineStats(
      role, aRole.getOrElse(role, ChampionStatistics.Statistics()),
      bRole.getOrElse(role, ChampionStatistics.Statistics())
    )
    ChampionStatistics(statistics = combinedList.toSeq)
  }

  def combineStats(role: Role, a: ChampionStatistics.Statistics, b: ChampionStatistics.Statistics): ChampionStatistics.Statistics = {
    val sums = for {
      aS <- a.sums
      bS <- b.sums
    } yield addSums(aS, bS)
    val quotients = sums.map(QuotientsGenerator.generateQuotients)
    val results = quotients.map(ResultsGenerator.generateResults)
    ChampionStatistics.Statistics(
      role = role,
      results = results,
      quotients = quotients,
      sums = sums
    )
  }

  def addSums(a: ChampionStatistics.Sums, b: ChampionStatistics.Sums): ChampionStatistics.Sums = {
    // TODO(igm): hard part
    ChampionStatistics.Sums()
  }

}
