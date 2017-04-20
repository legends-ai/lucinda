package asuna.lucinda.statistics

import asuna.common.legends.MomentsHelpers._
import asuna.proto.league.MatchSum
import MatchSum.Statistics.Moments
import asuna.proto.league.lucinda.AllChampionStatistics.Sums
import cats.Monoid
import cats.implicits._
import shapeless._

trait SumConverter[In, Out] {
  def convert(in: In, champion: Int): Out
}

object SumConverter {

  def apply[In, Out](implicit converter: SumConverter[In, Out]) = converter

  def instance[In, Out](f: (In, Int) => Out): SumConverter[In, Out] = new SumConverter[In, Out] {
    def convert(in: In, champion: Int): Out = f(in, champion)
  }

}

object SumConversionHelpers {

  implicit def mapConverter[T] = SumConverter.instance[T, Map[Int, T]] {
    (in, champion) => Map(champion -> in)
  }

  implicit val momentsConverter = mapConverter[Moments]

  implicit def optionMonoidConverter[T, U](
    implicit monoid: Monoid[T],
    conv: SumConverter[T, U]
  ): SumConverter[Option[T], U] = new SumConverter[Option[T], U] {

    override def convert(in: Option[T], champion: Int): U = {
      conv.convert(in.orEmpty, champion)
    }

  }

  implicit def optionConverter[T, U](
    implicit conv: SumConverter[T, U]
  ): SumConverter[Option[T], Option[U]] = new SumConverter[Option[T], Option[U]] {

    override def convert(in: Option[T], champion: Int): Option[U] = {
      in.map(v => conv.convert(v, champion))
    }

  }

  implicit val momentsOptConverter = implicitly[SumConverter[Option[Moments], Map[Int, Moments]]]

  implicit val dragonStatSeqConverter = SumConverter.instance[
    Seq[MatchSum.Statistics.Scalars.DragonStat], Seq[Sums.Scalars.DragonStat]
  ] { (sums, champ) =>
    sums.map { sum =>
      Sums.Scalars.DragonStat(
        dragon = sum.dragon,
        value = momentsOptConverter.convert(sum.value, champ)
      )
    }
  }

  implicit val subscalarMapConverter = SumConverter.instance[
    Map[Int, MatchSum.Collections.Subscalars], Map[Int, Sums.Subscalars.Subscalar]
  ] { (sum, champ) =>
    Map(
      champ -> Sums.Subscalars.Subscalar(
        plays = sum.mapValues(_.plays),
        wins = sum.mapValues(_.wins)
      )
    )
  }

  implicit val hnilConverter = SumConverter.instance[HNil, HNil] { (_, _) => HNil }

  implicit def hlistConverter[H, T <: HList, J, U <: HList](
    implicit hconv: SumConverter[H, J],
    tconv: SumConverter[T, U]
  ): SumConverter[H :: T, J :: U] = new SumConverter[H :: T, J :: U] {
    override def convert(in: H :: T, champion: Int): J :: U = {
      hconv.convert(in.head, champion) :: tconv.convert(in.tail, champion)
    }
  }

  implicit def hlistableConverter[T, U, TR <: HList, UR <: HList](
    implicit gent: Generic.Aux[T, TR],
    genu: Generic.Aux[U, UR],
    convt: SumConverter[TR, UR]
  ): SumConverter[T, U] = SumConverter.instance { (in, champ) =>
    genu.from(convt.convert(gent.to(in), champ))
  }

  implicit val scalarsConv = SumConverter[MatchSum.Statistics.Scalars, Sums.Scalars]
  implicit val deltaConv = SumConverter[MatchSum.Statistics.Deltas.Delta, Sums.Deltas.Delta]
  implicit val deltasConv = SumConverter[MatchSum.Statistics.Deltas, Sums.Deltas]
  implicit val ssConv = SumConverter[MatchSum.Collections.Subscalars, Sums.Subscalars.Subscalar]


  implicit object matchSumConverter extends SumConverter[MatchSum, Sums] {

    def convert(in: MatchSum, champion: Int): Sums = {
      Sums(
        plays = mapConverter[Int]
          .convert(in.statistics.map(_.plays).orEmpty, champion),
        scalars = in.statistics.flatMap(_.scalars).map { scalars =>
          scalarsConv.convert(scalars, champion)
        },
        deltas = in.statistics.flatMap(_.deltas).map { deltas =>
          deltasConv.convert(deltas, champion)
        },
        subscalars = in.collections.map { colls =>
          Sums.Subscalars(
            bans = subscalarMapConverter.convert(colls.bans, champion),
            allies = subscalarMapConverter.convert(colls.allies, champion)
          )
        }
      )
    }

  }

  implicit class MatchSumConversion(sum: MatchSum) {

    def asAggregate(champion: Int): Sums = {
      matchSumConverter.convert(sum, champion)
    }

  }

}
