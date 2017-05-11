package asuna.lucinda.statistics

import asuna.proto.league.lucinda.Statistic
import asuna.proto.league.MatchSum.Statistics.{ Moments => SMoments }
import asuna.proto.league.lucinda.AllChampionStatistics.{ Results, Sums }
import cats.implicits._
import shapeless._

trait ResultsDeriver[S, R] {
  def derive(sums: S): R
}

// TODO(igm): merge with Divisor somehow -- we don't really need the quotients object anymore
object ResultsDeriver {
  import asuna.common.legends.MomentsHelpers._

  def from[S, R](f: S => R): ResultsDeriver[S, R] = new ResultsDeriver[S, R] {
    def derive(sums: S): R = f(sums)
  }

  implicit val momentsDeriver = ResultsDeriver.from[
    Map[Int, SMoments], Map[Int, Statistic]
  ] { sum =>
    val quot = sum.mapValues(_.toQuotient)
    val sortedPairs = quot.toSeq.sortBy(_._2.mean).reverse

    // (weighted) average of the value across entire pairs map
    val sums = sum.values.toList
    val meanAcrossRole = sums.map(_.sum).combineAll / sums.map(_.count).combineAll

    val statsWithIndex = sortedPairs.zipWithIndex.map { case ((champ, value), index) =>
      (champ, (value, index))
    }.toMap

    statsWithIndex.mapValues { case (value, index) =>
      Statistic(
        rank = index + 1,
        mean = value.mean,
        meanAcrossRole = meanAcrossRole,
        stdev = Math.sqrt(value.variance),
        percentile = 1 - index.toDouble / quot.size,
      )
    }
  }

  implicit def optionsDeriver[S, R](
    implicit deriver: ResultsDeriver[S, R]
  ): ResultsDeriver[Option[S], Option[R]] =
    ResultsDeriver.from[Option[S], Option[R]](_.map(x => deriver.derive(x)))

  implicit val dragonStatSeqDeriver = ResultsDeriver.from[
    Seq[Sums.Scalars.DragonStat], Seq[Results.Scalars.DragonStat]
  ] { sums =>
    sums.groupBy(_.dragon).mapValues(_.headOption).values.flatten.toSeq.map { sum =>
      Results.Scalars.DragonStat(
        dragon = sum.dragon,
        value = momentsDeriver.derive(sum.value)
      )
    }
  }


  implicit val hnilDeriver = ResultsDeriver.from[HNil, HNil] { _ => HNil }

  implicit def hlistDeriver[A, B <: HList, U, V <: HList](
    implicit hd: ResultsDeriver[A, U],
    td: ResultsDeriver[B, V]
  ): ResultsDeriver[A :: B, U :: V] = ResultsDeriver.from { sums =>
    hd.derive(sums.head) :: td.derive(sums.tail)
  }

  implicit def hlistableDeriver[A, AT <: HList, U, UT <: HList](
    implicit gena: Generic.Aux[A, AT],
    genu: Generic.Aux[U, UT],
    deriver: ResultsDeriver[AT, UT]
  ): ResultsDeriver[A, U] = ResultsDeriver.from { sums =>
    genu.from(deriver.derive(gena.to(sums)))
  }

  // need this for a compiler hint,
  // TODO(igm): remove
  implicit val deltaDeriver = implicitly[
    ResultsDeriver[Option[Sums.Deltas.Delta], Option[Results.Deltas.Delta]]]

  implicit val scalarsDeriver = implicitly[
    ResultsDeriver[Option[Sums.Scalars], Option[Results.Scalars]]]
  implicit val deltasDeriver = implicitly[
    ResultsDeriver[Option[Sums.Deltas], Option[Results.Deltas]]]

  private def derivePlays(in: Map[Int, Int])(
    implicit deriver: ResultsDeriver[Map[Int, SMoments], Map[Int, Statistic]]
  ): Map[Int, Statistic] = {
    val sums = in.mapValues { plays =>
      SMoments(
        count = 1,
        sum = plays,
      )
    }
    deriver.derive(sums)
  }

  implicit object FinalDeriver extends ResultsDeriver[Sums, Results] {

    def derive(sums: Sums): Results = {
      Results(
        plays = derivePlays(sums.plays),
        scalars = scalarsDeriver.derive(sums.scalars),
        deltas = deltasDeriver.derive(sums.deltas),
        derivatives = derivatives(sums).some,
      )
    }

    def derivatives(sums: Sums)(
      implicit deriver: ResultsDeriver[Map[Int, SMoments], Map[Int, Statistic]]
    ): Results.Derivatives = {
      // map of total games played per champion
      val plays = sums.plays

      val totalGames = plays.values.sum / 2

      // we will end up dividing each value by this total # of games to get incidence rate.
      val pickRateSums = plays.mapValues { v =>
        SMoments(
          count = totalGames,
          sum = v,
        )
      }

      val bans = sums.subscalars.map(_.bans).getOrElse(Map())

      // 6 bans per match. This finds us the total number of matches.
      // in theory, this should be the same as totalGames, but meh
      val total = bans.values.sum.toDouble / 6
      val banRateSums = bans.mapValues { v =>
        SMoments(
          count = total.toInt,
          sum = v,
        )
      }

      Results.Derivatives(
        picks = deriver.derive(pickRateSums),
        bans = deriver.derive(banRateSums),
      )
    }

  }

}

/**
  * Generates the Results part of the statistics.
  */
object ResultsGenerator {

  def generate(sums: Sums): Results =
    ResultsDeriver.FinalDeriver.derive(sums)
}
