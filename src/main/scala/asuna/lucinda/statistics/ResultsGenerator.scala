package asuna.lucinda.statistics

import asuna.proto.league.lucinda.Statistic
import asuna.proto.league.MatchSum.Statistics.{ Moments => SMoments }
import asuna.proto.league.lucinda.MatchQuotient.Statistics.{ Moments => QMoments }
import asuna.proto.league.lucinda.AllChampionStatistics.{ Results, Sums, Quotients }
import asuna.common.legends.MomentsHelpers._
import cats.implicits._
import shapeless._
import org.log4s._

trait ResultsDeriver[S, Q, R] {
  def derive(sums: S, quotients: Q): R
}

// TODO(igm): merge with Divisor somehow -- we don't really need the quotients object anymore
object ResultsDeriver {

  private[this] val logger = getLogger

  def from[S, Q, R](f: (S, Q) => R): ResultsDeriver[S, Q, R] = new ResultsDeriver[S, Q, R] {
    def derive(sums: S, quotients: Q): R = f(sums, quotients)
  }

  implicit val momentsDeriver = ResultsDeriver.from[
    Map[Int, SMoments], Map[Int, QMoments], Map[Int, Statistic]
  ] { (sum, quot) =>
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
        // TODO(igm): is this what we mean by percentile?
        percentile = 1 - index.toDouble / quot.size
      )
    }
  }

  implicit def optionsDeriver[S, Q, R](
    implicit deriver: ResultsDeriver[S, Q, R]
  ): ResultsDeriver[Option[S], Option[Q], Option[R]] =
    ResultsDeriver.from[Option[S], Option[Q], Option[R]] { (sumsOpt, quotientsOpt) =>
      (sumsOpt |@| quotientsOpt).map { (sums, quotients) =>
        deriver.derive(sums, quotients)
      }
    }

  implicit val dragonStatSeqDeriver = ResultsDeriver.from[
    Seq[Sums.Scalars.DragonStat], Seq[Quotients.Scalars.DragonStat], Seq[Results.Scalars.DragonStat]
  ] { (sums, quots) =>
    val sumsMap = sums.groupBy(_.dragon)
    val quotsMap = quots.groupBy(_.dragon)
    (sumsMap.keySet ++ quotsMap.keySet).toSeq.map { dragon =>
      (sumsMap.get(dragon).flatMap(_.headOption) |@| quotsMap.get(dragon).flatMap(_.headOption)) map { (sum, quot) =>
        Results.Scalars.DragonStat(
          dragon = sum.dragon,
          value = momentsDeriver.derive(sum.value, quot.value)
        )
      }
    }.flatten
  }


  implicit val hnilDeriver = ResultsDeriver.from[HNil, HNil, HNil] { (_, _) => HNil }

  implicit def hlistDeriver[A, B <: HList, C, D <: HList, U, V <: HList](
    implicit hd: ResultsDeriver[A, C, U],
    td: ResultsDeriver[B, D, V]
  ): ResultsDeriver[A :: B, C :: D, U :: V] = ResultsDeriver.from { (sums, quotients) =>
    hd.derive(sums.head, quotients.head) :: td.derive(sums.tail, quotients.tail)
  }

  implicit def hlistableDeriver[A, AT <: HList, C, CT <: HList, U, UT <: HList](
    implicit gena: Generic.Aux[A, AT],
    genc: Generic.Aux[C, CT],
    genu: Generic.Aux[U, UT],
    deriver: ResultsDeriver[AT, CT, UT]
  ): ResultsDeriver[A, C, U] = ResultsDeriver.from { (sums, quotients) =>
    genu.from(deriver.derive(gena.to(sums), genc.to(quotients)))
  }

  // need this for a compiler hint,
  // TODO(igm): remove
  implicit val deltaDeriver = implicitly[
    ResultsDeriver[Option[Sums.Deltas.Delta], Option[Quotients.Deltas.Delta], Option[Results.Deltas.Delta]]]

  implicit val scalarsDeriver = implicitly[
    ResultsDeriver[Option[Sums.Scalars], Option[Quotients.Scalars], Option[Results.Scalars]]]
  implicit val deltasDeriver = implicitly[
    ResultsDeriver[Option[Sums.Deltas], Option[Quotients.Deltas], Option[Results.Deltas]]]

  private def derivePlays(in: Map[Int, Int])(
    implicit deriver: ResultsDeriver[Map[Int, SMoments], Map[Int, QMoments], Map[Int, Statistic]]
  ): Map[Int, Statistic] = {
    val sums = in.mapValues { plays =>
      SMoments(
        count = 1,
        sum = plays,
      )
    }
    deriver.derive(sums, sums.mapValues(_.toQuotient))
  }

  class FinalDeriver(roleCount: Int) extends ResultsDeriver[Sums, Quotients, Results] {

    def derive(sums: Sums, quotients: Quotients): Results = {
      Results(
        plays = derivePlays(sums.plays),
        scalars = scalarsDeriver.derive(sums.scalars, quotients.scalars),
        deltas = deltasDeriver.derive(sums.deltas, quotients.deltas),
        derivatives = derivatives(sums).some,
      )
    }

    def derivatives(sums: Sums)(
      implicit deriver: ResultsDeriver[Map[Int, SMoments], Map[Int, QMoments], Map[Int, Statistic]]
    ): Results.Derivatives = {
      // map of total games played per champion
      val plays = sums.plays

      // total number of champion instances analyzed.
      // if divisor is zero, that is a big problem. We will warn.
      if (roleCount === 0) {
        logger.warn("zero role count encountered...")
      }
      val totalGames = plays.values.sum / Math.max(2 * roleCount, 1)

      // we will end up dividing each value by this total # of games to get incidence rate.
      val pickRateSums = plays.mapValues { v =>
        SMoments(
          count = totalGames,
          sum = v,
        )
      }

      val banCount = sums.subscalars.map(_.bans.mapValues(_.plays)).orEmpty
      val bans = banCount.mapValues(_.values.toList.combineAll)

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
        picks = deriver.derive(pickRateSums, pickRateSums.mapValues(_.toQuotient)),
        bans = deriver.derive(banRateSums, banRateSums.mapValues(_.toQuotient))
      )
    }

  }

}

/**
  * Generates the Results part of the statistics.
  */
object ResultsGenerator {

  def generate(roleCount: Int, sums: Sums, quotients: Quotients): Results =
    new ResultsDeriver.FinalDeriver(roleCount).derive(sums, quotients)
}
