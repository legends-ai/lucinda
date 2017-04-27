package asuna.lucinda.matches

import cats.implicits._
import scala.language.implicitConversions
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import asuna.proto.league.MatchSum.{ Collections => SC }
import asuna.proto.league.lucinda.MatchQuotient.{ Collections => QC }
import asuna.common.legends.MomentsHelpers._
import SubscalarsMapping._
import shapeless._

object Divisible {

  /**
    * Allows running .divide on any Divisor.
    */
  implicit class Ops[SumT](sum: SumT) {
    /**
      * Applies a divisor.
      */
    def quotient[QuotT](implicit divisor: Divisor[SumT, QuotT]): QuotT = divisor.divide(sum)

    /**
      * Uses the composite divisor.
      */
    def quotientC[QuotT, SumT2, QuotT2](divisor: Divisor[SumT2, QuotT2])(
      implicit compositeDivisor: CompositeDivisor[SumT, QuotT, SumT2, QuotT2],
    ): QuotT = compositeDivisor.divide(sum, divisor)

  }

}

/**
  * Divides a type to produce a quotient.
  */
trait Divisor[SumT, QuotT] {
  def divide(sum: SumT): QuotT
}

/**
  * A divisor composed of another divisor.
  * The alternative is to allocate another divisor every single time this is called,
  * but that could end up being pretty poor for performance.
  */
trait CompositeDivisor[SumT, QuotT, SumT2, QuotT2] {
  def divide(sum: SumT, count: Divisor[SumT2, QuotT2]): QuotT
}

object CompositeDivisor {
  import Divisible._

  def apply[S, Q, S2, Q2](implicit cd: CompositeDivisor[S, Q, S2, Q2]) = cd

  def instance[S, Q, S2, Q2](f: (S, Divisor[S2, Q2]) => Q): CompositeDivisor[S, Q, S2, Q2] =
    new CompositeDivisor[S, Q, S2, Q2] {
      def divide(sum: S, count: Divisor[S2, Q2]): Q = f(sum, count)
    }

  implicit val subscalarDivisor: CompositeDivisor[SC.Subscalars, QC.Subscalars, Int, Double] = instance { (sum, countDivisor) =>
    QC.Subscalars(
      playRate = sum.plays.quotient(countDivisor),
      winRate = RealDivisor(sum.plays).divide(sum.wins.toDouble),
      playCount = sum.plays.toInt,
    )
  }

  implicit def compositeOptionDivisor[SumT, QuotT, SumT2, QuotT2](
    implicit div: CompositeDivisor[SumT, QuotT, SumT2, QuotT2]
  ): CompositeDivisor[Option[SumT], Option[QuotT], SumT2, QuotT2] = instance { (sum, count) =>
    sum.map(s => div.divide(s, count))
  }

  implicit def subscalarsMapDivisor[T]: CompositeDivisor[
    Map[T, MatchSum.Collections.Subscalars],
    Map[T, MatchQuotient.Collections.Subscalars],
    Int, Double,
  ] = instance { (sum, ct) =>
    sum.mapValues(_.quotientC(ct))
  }

  implicit def collectionDivisor[SumT, QuotT](implicit extractor: SubscalarsMapping[SumT, QuotT]): CompositeDivisor[SumT, QuotT, Int, Double] = instance { (sum, ct) =>
    val ss = extractor.extract(sum)
    extractor.build(sum, ss.map(_.quotientC(ct)))
  }

  implicit val otherChampionStatsDivisor: CompositeDivisor[
    MatchSum.Collections.OtherChampionStats,
    MatchQuotient.Collections.OtherChampionStats,
    Int, Double,
  ] = instance { (sum, count) =>
    MatchQuotient.Collections.OtherChampionStats(
      role = sum.role,
      stats = sum.stats.mapValues(_.quotientC(count)),
    )
  }

  implicit def collectionSeqDivisor[SumT, QuotT](
    implicit indiv: CompositeDivisor[SumT, QuotT, Int, Double],
  ): CompositeDivisor[Seq[SumT], Seq[QuotT], Int, Double] = instance { (sum, ct) =>
    sum.map(_.quotientC(ct))
  }

  implicit object CollectionsDivisor extends CompositeDivisor[MatchSum.Collections, MatchQuotient.Collections, Int, Double] {
    def divide(sum: MatchSum.Collections, ct: Divisor[Int, Double]): MatchQuotient.Collections = {
      MatchQuotient.Collections(
        masteries = sum.masteries.quotientC(ct),
        runes = sum.runes.quotientC(ct),
        keystones = sum.keystones.quotientC(ct),
        summoners = sum.summoners.quotientC(ct),
        startingTrinkets = sum.startingTrinkets.quotientC(ct),
        endingTrinkets = sum.endingTrinkets.quotientC(ct),
        skillOrders = sum.skillOrders.quotientC(ct),
        durations = sum.durations.quotientC(ct),
        bans = sum.bans.quotientC(ct),
        allies = sum.allies.quotientC(ct),
        enemies = sum.enemies.quotientC(ct),
        starterItems = sum.starterItems.quotientC(ct),
        coreBuilds = sum.coreBuilds.quotientC(ct),
        experienceDistribution = sum.experienceDistribution.quotientC(ct),
        teams = sum.teams.quotientC(ct),
      )
    }
  }

}

/**
  * A divisor that operates on real numbers.
  */
case class RealDivisor(divisor: Int) extends Divisor[Double, Double] {

  override def divide(dividend: Double): Double =
    if (divisor != 0) dividend / divisor.toDouble else 0

}

object Divisor {
  import CompositeDivisor._
  import Divisible._

  def apply[S, Q](implicit divisor: Divisor[S, Q]) = divisor

  def instance[S, D](f: S => D): Divisor[S, D] = new Divisor[S, D] {
    def divide(sum: S): D = f(sum)
  }

  implicit def numericRealDivisor[T](div: Divisor[Double, Double])(implicit num: Numeric[T]): Divisor[T, Double] = {
    new Divisor[T, Double] {
      override def divide(dividend: T): Double = div.divide(num.toDouble(dividend))
    }
  }

  implicit def optionDivisor[SumT, QuotT](implicit div: Divisor[SumT, QuotT]): Divisor[Option[SumT], Option[QuotT]] = {
    new Divisor[Option[SumT], Option[QuotT]] {
      override def divide(sum: Option[SumT]): Option[QuotT] = {
        sum.map(div.divide)
      }
    }
  }

  implicit object MomentsDivisor extends Divisor[MatchSum.Statistics.Moments, MatchQuotient.Statistics.Moments] {
    def divide(sum: MatchSum.Statistics.Moments): MatchQuotient.Statistics.Moments = {
      sum.toQuotient
    }
  }

  implicit object DragonStatDivisor extends Divisor[MatchSum.Statistics.Scalars.DragonStat, MatchQuotient.Statistics.Scalars.DragonStat] {
    def divide(sum: MatchSum.Statistics.Scalars.DragonStat) = MatchQuotient.Statistics.Scalars.DragonStat(
      dragon = sum.dragon,
      value = sum.value.quotient
    )
  }

  implicit val kdaDistributionDivisor: Divisor[Seq[MatchSum.Collections.KDADistribution], Seq[MatchQuotient.Collections.KDADistribution]] =
    instance { sum =>
      val totalKills = sum.map(_.kills).sum
      val totalDeaths = sum.map(_.deaths).sum
      val totalAssists = sum.map(_.assists).sum
      val totalExecutes = sum.map(_.executes).sum
      sum.map { elt =>
        MatchQuotient.Collections.KDADistribution(
          time = elt.time,

          kills = if (totalKills > 0) elt.kills / totalKills else 0,
          deaths = if (totalDeaths > 0) elt.deaths / totalDeaths else 0,
          assists = if (totalAssists > 0) elt.assists / totalAssists else 0,
          executes = if (totalExecutes > 0) elt.executes / totalExecutes else 0,
        )
      }
    }

  implicit val momentsOptionDivisor = optionDivisor(MomentsDivisor)

  implicit val hnilDivisor = instance[HNil, HNil](identity)

  implicit def hlistDivisor[H, T <: HList, J, U <: HList](
    implicit hdiv: Divisor[H, J],
    tdiv: Divisor[T, U]
  ): Divisor[H :: T, J :: U] = instance { case (head :: tail) =>
    hdiv.divide(head) :: tdiv.divide(tail)
  }

  implicit def genericDivisor[T, U, TR <: HList, UR <: HList](
    implicit gent: Generic.Aux[T, TR],
    genu: Generic.Aux[U, UR],
    div: Divisor[TR, UR]
  ): Divisor[T, U] = instance { in =>
    genu.from(div.divide(gent.to(in)))
  }

  implicit def seqDivisor[S, Q](
    implicit div: Divisor[S, Q],
  ): Divisor[Seq[S], Seq[Q]] = instance { in =>
    in.map(_.quotient)
  }

  // TODO(igm): investigate shapeless Lazy
  // this is ugly af and appears way too many times in our codebase
  // kinda defeats the point

  implicit val scalarsOptDivisor = Divisor[Option[MatchSum.Statistics.Scalars], Option[MatchQuotient.Statistics.Scalars]]
  implicit val deltaOptDivisor = Divisor[Option[MatchSum.Statistics.Deltas.Delta], Option[MatchQuotient.Statistics.Deltas.Delta]]
  implicit val deltasOptDivisor = Divisor[Option[MatchSum.Statistics.Deltas], Option[MatchQuotient.Statistics.Deltas]]

  implicit object StatisticsDivisor extends Divisor[MatchSum.Statistics, MatchQuotient.Statistics] {
    def divide(sum: MatchSum.Statistics): MatchQuotient.Statistics = {
      MatchQuotient.Statistics(
        plays = sum.plays,
        scalars = sum.scalars.quotient,
        deltas = sum.deltas.quotient,
      )
    }
  }

  implicit object MatchDivisor extends Divisor[MatchSum, MatchQuotient] {
    def divide(sum: MatchSum): MatchQuotient = {
      implicit val ct = RealDivisor(sum.statistics.map(_.plays).orEmpty)
      // im tilted af
      // we should have an implicit def here but scala is dumb
      implicit val ctInt = numericRealDivisor[Int](ct)
      MatchQuotient(
        statistics = sum.statistics.quotient,
        collections = sum.collections.quotientC(ctInt),
      )
    }
  }

}
