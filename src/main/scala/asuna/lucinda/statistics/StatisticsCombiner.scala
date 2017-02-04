package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics
import cats.Monoid
import cats.implicits._

import SumsHelpers._

object StatisticsCombiner {

  implicit object StatisticsMonoid extends Monoid[AllChampionStatistics] {

    def combine(a: AllChampionStatistics, b: AllChampionStatistics): AllChampionStatistics = {
      // wow a monoid can calculate things too!
      val sums = a.sums |+| b.sums
      val quotients = sums.map(QuotientsGenerator.generateQuotients)
      val results = (sums |@| quotients).map(ResultsGenerator).map(_.generate)
      AllChampionStatistics(
        // Roles should be the same. If they're not, fuck my ass.
        role = a.role,
        results = results,
        quotients = quotients,
        sums = sums
      )
    }

    def empty = AllChampionStatistics()

  }

}
