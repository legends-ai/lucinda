package asuna.lucinda.statistics

import scala.language.implicitConversions
import asuna.proto.league.lucinda.Statistic
import asuna.proto.league.MatchSum
import asuna.proto.league.MatchSum.Statistics.{ Moments => SMoments }
import asuna.proto.league.lucinda.MatchQuotient.Statistics.{ Moments => QMoments }
import asuna.proto.league.lucinda.AllChampionStatistics.{ Results, Sums, Quotients }
import cats.implicits._
import shapeless._

trait ResultsDeriver[S, Q, R] {
  def derive(sums: S, quotients: Q): R
}

object ResultsDeriver {

  def from[S, Q, R](f: (S, Q) => R): ResultsDeriver[S, Q, R] = new ResultsDeriver[S, Q, R] {
    def derive(sums: S, quotients: Q): R = f(sums, quotients)
  }

  implicit val momentsDeriver = ResultsDeriver.from[
    Map[Int, SMoments], Map[Int, QMoments], Map[Int, Statistic]
  ] { (sum, quot) =>
    val sortedPairs = quot.toSeq.sortBy(_._2.mean).reverse

    // (weighted) average of the value across entire pairs map
    val meanAcrossRole = sum.values.toList.map(_.sum).combineAll / sum.size

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

  implicit object finalDeriver extends ResultsDeriver[Sums, Quotients, Results] {
    // need this for a compiler hint,
    // TODO(igm): remove
    implicit val deltaDeriver = implicitly[
      ResultsDeriver[Option[Sums.Deltas.Delta], Option[Quotients.Deltas.Delta], Option[Results.Deltas.Delta]]]

    val scalarsDeriver = implicitly[
      ResultsDeriver[Option[Sums.Scalars], Option[Quotients.Scalars], Option[Results.Scalars]]]
    val deltasDeriver = implicitly[
      ResultsDeriver[Option[Sums.Deltas], Option[Quotients.Deltas], Option[Results.Deltas]]]

    def derive(sums: Sums, quotients: Quotients): Results = {
      Results(
        scalars = scalarsDeriver.derive(sums.scalars, quotients.scalars),
        deltas = deltasDeriver.derive(sums.deltas, quotients.deltas),
        derivatives = derivatives(sums).some
      )
    }

    def derivatives(sums: Sums): Results.Derivatives = {
      // map of total games played per champion
      val plays = sums.plays

      // total number of games. div by 10 since 10 champs per game.
      // TODO(igm): tweak based off game mode. twisted treeline?
      val totalGames = plays.values.sum / 10
      val pickRateMap = plays.mapValues(_.toDouble / totalGames)

      val banRateMap = for {
        banCount <- sums.subscalars.map(_.bans.mapValues(_.plays))
      } yield {
        val bans = banCount.mapValues(_.values.toList.combineAll)
        // 6 bans per match. This finds us the total number of matches.
        val total = bans.values.sum.toDouble / 6
        bans.mapValues(_.toDouble / total)
      }
      Results.Derivatives(
        // picks = makeStat(pickRateMap, plays),
        // bans = makeStat(banRateMap.orEmpty, plays)
      )
    }

  }

}

/**
  * Generats the Results part of the statistics.
  */
case class ResultsGenerator(sums: Sums, quotients: Quotients) {

  def generate(): Results = implicitly[ResultsDeriver[Sums, Quotients, Results]].derive(sums, quotients)

}
