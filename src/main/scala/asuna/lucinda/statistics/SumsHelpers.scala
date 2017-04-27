package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics
import AllChampionStatistics.Sums.Scalars.DragonStat
import AllChampionStatistics.Sums.Subscalars.PickStats
import cats._
import cats.implicits._
import cats.data.NonEmptyList
import cats.derived._, monoid._, legacy._

object SumsHelpers {

  implicit object dragonStatSemigroup extends Semigroup[DragonStat] {
    def combine(a: DragonStat, b: DragonStat): DragonStat = {
      a.copy(value = a.value |+| b.value)
    }
  }

  implicit object dragonSumMonoid extends Monoid[Seq[AllChampionStatistics.Sums.Scalars.DragonStat]] {

    def combine(a: Seq[DragonStat], b: Seq[DragonStat]): Seq[DragonStat] = {
      (a ++ b)
        // same dragon
        .groupBy(_.dragon)
        .mapValues(x => NonEmptyList.fromList(x.toList).map(_.reduce.some).getOrElse(None))
        .values.flatten.toSeq
    }

    def empty = Seq()

  }

  implicit object pickStatsSemigroup extends Semigroup[PickStats] {
    def combine(a: PickStats, b: PickStats): PickStats =
      a.copy(picks = a.picks |+| b.picks)
  }

  implicit object pickStatsMonoid extends Monoid[Seq[PickStats]] {

    def combine(a: Seq[PickStats], b: Seq[PickStats]): Seq[PickStats] = {
      (a ++ b)
        .groupBy(_.role)
        .mapValues(x => NonEmptyList.fromList(x.toList).map(_.reduce.some).getOrElse(None))
        .values.flatten.toSeq
    }

    def empty = Seq()

  }

  implicit val SumsMonoid = Monoid[AllChampionStatistics.Sums]

}
