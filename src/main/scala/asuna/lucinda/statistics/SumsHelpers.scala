package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics
import cats.Monoid
import cats.implicits._
import cats.derived._, monoid._, legacy._

object SumsHelpers {

  implicit object dragonStatMonoid extends Monoid[AllChampionStatistics.Sums.Scalars.DragonStat] {

    def combine(
      a: AllChampionStatistics.Sums.Scalars.DragonStat,
      b: AllChampionStatistics.Sums.Scalars.DragonStat
    ): AllChampionStatistics.Sums.Scalars.DragonStat = {
      a.copy(value = a.value |+| b.value)
    }

    def empty = AllChampionStatistics.Sums.Scalars.DragonStat()

  }

  implicit object dragonSumMonoid extends Monoid[Seq[AllChampionStatistics.Sums.Scalars.DragonStat]] {

    def combine(
      a: Seq[AllChampionStatistics.Sums.Scalars.DragonStat],
      b: Seq[AllChampionStatistics.Sums.Scalars.DragonStat]
    ): Seq[AllChampionStatistics.Sums.Scalars.DragonStat] = {
      (a ++ b)
        // same dragon
        .groupBy(_.dragon)
        // get values as a seq
        .values.toSeq
        // combine all stats with same dragon
        .map(_.toList.combineAll)
    }

    def empty = Seq()

  }

  implicit val SumsMonoid = Monoid[AllChampionStatistics.Sums]

}
