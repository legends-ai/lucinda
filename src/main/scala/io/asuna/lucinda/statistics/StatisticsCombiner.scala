package io.asuna.lucinda.statistics

import SumsHelpers._
import cats.Monoid
import cats.implicits._
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics

object StatisticsCombiner {

  implicit object StatisticsMonoid extends Monoid[ChampionStatistics] {

    def combine(a: ChampionStatistics, b: ChampionStatistics): ChampionStatistics = {
      // wow a monoid can calculate things too!
      val sums = a.sums |+| b.sums
      val quotients = sums.map(QuotientsGenerator.generateQuotients)
      val results = (sums |@| quotients).map(ResultsGenerator).map(_.generate)
      ChampionStatistics(
        // Roles should be the same. If they're not, fuck my ass.
        role = a.role,
        results = results,
        quotients = quotients,
        sums = sums
      )
    }

    def empty = ChampionStatistics()

  }

}
