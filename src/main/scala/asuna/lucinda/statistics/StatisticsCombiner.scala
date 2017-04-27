package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics
import cats.Monoid
import cats.implicits._

import SumsHelpers._

object StatisticsCombiner {

  implicit object StatisticsMonoid extends Monoid[AllChampionStatistics] {

    def combine(a: AllChampionStatistics, b: AllChampionStatistics): AllChampionStatistics = {
      val sums = a.sums |+| b.sums
      val results = sums.map { sms =>
        ResultsGenerator.generate(sms)
      }
      AllChampionStatistics(
        // Roles should be the same. If they're not, fuck my ass.
        results = results,
        sums = sums,
      )
    }

    def empty = AllChampionStatistics()

  }

}
