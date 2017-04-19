package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics
import cats.Monoid
import cats.implicits._
import org.log4s._

import SumsHelpers._

object StatisticsCombiner {

  private[this] val logger = getLogger

  implicit object StatisticsMonoid extends Monoid[AllChampionStatistics] {

    def combine(a: AllChampionStatistics, b: AllChampionStatistics): AllChampionStatistics = {
      val sums = a.sums |+| b.sums
      val quotients = sums.map(QuotientsGenerator.generateQuotients)
      val results = (sums |@| quotients).map { (sums, quotients) =>
        // note that this combination is invalid if the role counts are different...
        // there isn't really a good way to enforce this other than throwing
        // an exception, so we'll just log and allow it :)
        if (a.roleCount =!= b.roleCount) {
          logger.warn(s"role counts ${a.roleCount} and ${b.roleCount} don't match up!")
        }
        ResultsGenerator.generate(a.roleCount, sums, quotients)
      }
      AllChampionStatistics(
        // Roles should be the same. If they're not, fuck my ass.
        results = results,
        quotients = quotients,
        sums = sums,
        roleCount = a.roleCount,
      )
    }

    def empty = AllChampionStatistics()

  }

}
