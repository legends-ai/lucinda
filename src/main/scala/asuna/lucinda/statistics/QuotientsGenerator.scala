package asuna.lucinda.statistics

import scala.math.Numeric
import scala.math.Numeric.Implicits._
import asuna.proto.league.MatchSum.Statistics.{ Moments => SMoments }
import asuna.proto.league.lucinda.AllChampionStatistics.{ Quotients, Sums }
import asuna.proto.league.lucinda.MatchQuotient.Statistics.{ Moments => QMoments }
import cats.implicits._
import shapeless.{ Generic, HList, ::, HNil, Lazy }

trait QuotientsDeriver[S, Q] {
  def derive(sums: S): Q
}

object QuotientsDeriver {

  def apply[S, Q](implicit deriver: QuotientsDeriver[S, Q]) = deriver

  def pure[S, Q](f: S => Q) = new QuotientsDeriver[S, Q] {
    def derive(sums: S): Q = f(sums)
  }

  import asuna.common.legends.MomentsHelpers._

  implicit val momentsDeriver =
    pure[Map[Int, SMoments], Map[Int, QMoments]](_.mapValues(_.toQuotient))

  implicit val dragonsDeriver =
    pure[Seq[Sums.Scalars.DragonStat], Seq[Quotients.Scalars.DragonStat]] { dragons =>
      dragons map { stat =>
        Quotients.Scalars.DragonStat(
          dragon = stat.dragon,
          // TODO(p): Figure out if there is a way for shapeless to derive this implicitly
          value = momentsDeriver.derive(stat.value)
        )
      }
    }

  implicit def optionDeriver[S, Q](implicit deriver: QuotientsDeriver[S, Q]) =
    pure[Option[S], Option[Q]](_.map(deriver.derive))

  implicit val hnilDeriver = pure[HNil, HNil](_ => HNil)

  implicit def hlistDeriver[SH, ST <: HList, QH, QT <: HList](
    implicit
    hDeriver: Lazy[QuotientsDeriver[SH, QH]],
    tDeriver: Lazy[QuotientsDeriver[ST, QT]]
  ) = pure[SH :: ST, QH :: QT] { case (head :: tail) =>
    hDeriver.value.derive(head) :: tDeriver.value.derive(tail)
  }

  implicit def genericDeriver[S, Q, RS, RQ](
    implicit
    sGen: Generic.Aux[S, RS],
    qGen: Generic.Aux[Q, RQ],
    deriver: QuotientsDeriver[RS, RQ]
  ) = pure[S, Q](sums => qGen.from(deriver.derive(sGen.to(sums))))

  implicit object generateDeriver extends QuotientsDeriver[Sums, Quotients] {

    def derive(sums: Sums): Quotients = {
      val scalarsDeriver = QuotientsDeriver[Sums.Scalars, Quotients.Scalars]

      implicit val deltaDeriver = QuotientsDeriver[Sums.Deltas.Delta, Quotients.Deltas.Delta]
      val deltasDeriver = QuotientsDeriver[Sums.Deltas, Quotients.Deltas]

      Quotients(
        scalars = sums.scalars
          .map(scalarsDeriver.derive)
          .getOrElse(Quotients.Scalars())
          .some,
        deltas = sums.deltas
          .map(deltasDeriver.derive)
          .getOrElse(Quotients.Deltas())
          .some
      )
    }

  }

}

object QuotientsGenerator {

  def generateQuotients(sums: Sums): Quotients =
    QuotientsDeriver[Sums, Quotients].derive(sums)

}
