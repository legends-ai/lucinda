package asuna.lucinda.matches

import cats.Monoid
import cats.implicits._
import scala.language.implicitConversions
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import asuna.proto.league.MatchSum.{ Collections => SC }
import asuna.proto.league.lucinda.MatchQuotient.{ Collections => QC }
import asuna.common.legends.MatchSumHelpers._
import asuna.common.legends.MomentsHelpers._
import SubscalarsMapping._


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
  def divide(sum: SumT)(implicit count: Divisor[SumT2, QuotT2]): QuotT
}

/**
  * A divisor that operates on real numbers.
  */
case class RealDivisor(divisor: Int) extends Divisor[Double, Double] {

  override def divide(dividend: Double): Double =
    if (divisor != 0) dividend / divisor.toDouble else 0

}

object Divisor {

  implicit def numericRealDivisor[T](div: Divisor[Double, Double])(implicit num: Numeric[T]): Divisor[T, Double] = {
    new Divisor[T, Double] {
      override def divide(dividend: T): Double = div.divide(num.toDouble(dividend))
    }
  }

  implicit def compositeOptionDivisor[SumT, QuotT, SumT2, QuotT2](
    implicit div: CompositeDivisor[SumT, QuotT, SumT2, QuotT2]
  ): CompositeDivisor[Option[SumT], Option[QuotT], SumT2, QuotT2] = {
    new CompositeDivisor[Option[SumT], Option[QuotT], SumT2, QuotT2] {
      override def divide(sum: Option[SumT])(implicit count: Divisor[SumT2, QuotT2]): Option[QuotT] = {
        sum.map(div.divide)
      }
    }
  }

  implicit object SubscalarDivisor extends CompositeDivisor[SC.Subscalars, QC.Subscalars, Int, Double] {
    def divide(sum: SC.Subscalars)(implicit countDivisor: Divisor[Int, Double]): QC.Subscalars = {
      QC.Subscalars(
        playRate = sum.plays.quotient,
        winRate = RealDivisor(sum.plays).divide(sum.wins.toDouble),
        playCount = sum.plays.toInt
      )
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

  implicit val momentsOptionDivisor = optionDivisor(MomentsDivisor)

  implicit object ScalarsDivisor extends Divisor[MatchSum.Statistics.Scalars, MatchQuotient.Statistics.Scalars] {
    def divide(sum: MatchSum.Statistics.Scalars): MatchQuotient.Statistics.Scalars = {
      MatchQuotient.Statistics.Scalars(
        wins = sum.wins.quotient,
        goldEarned = sum.goldEarned.quotient,
        kills = sum.kills.quotient,
        deaths = sum.deaths.quotient,
        assists = sum.assists.quotient,
        damageDealt = sum.damageDealt.quotient,
        damageTaken = sum.damageTaken.quotient,
        minionsKilled = sum.minionsKilled.quotient,
        teamJungleMinionsKilled = sum.teamJungleMinionsKilled.quotient,
        enemyJungleMinionsKilled = sum.enemyJungleMinionsKilled.quotient,
        killingSpree = sum.killingSpree.quotient,
        wardsBought = sum.wardsBought.quotient,
        wardsPlaced = sum.wardsPlaced.quotient,
        wardsKilled = sum.wardsKilled.quotient,
        crowdControl = sum.crowdControl.quotient,
        firstBlood = sum.firstBlood.quotient,
        firstBloodAssist = sum.firstBloodAssist.quotient,
        doublekills = sum.doublekills.quotient,
        triplekills = sum.triplekills.quotient,
        quadrakills = sum.quadrakills.quotient,
        pentakills = sum.pentakills.quotient,
        physicalDamage = sum.physicalDamage.quotient,
        magicDamage = sum.magicDamage.quotient,
        trueDamage = sum.trueDamage.quotient
      )
    }
  }

  implicit val scalarsOptDivisor = optionDivisor(ScalarsDivisor)

  implicit object DeltaDivisor
      extends Divisor[MatchSum.Statistics.Deltas.Delta, MatchQuotient.Statistics.Deltas.Delta] {
    def divide(sum: MatchSum.Statistics.Deltas.Delta): MatchQuotient.Statistics.Deltas.Delta = {
      MatchQuotient.Statistics.Deltas.Delta(
        zeroToTen = sum.zeroToTen.quotient,
        tenToTwenty = sum.tenToTwenty.quotient,
        twentyToThirty = sum.twentyToThirty.quotient,
        thirtyToEnd = sum.thirtyToEnd.quotient
      )
    }
  }

  implicit val deltaOptDivisor = optionDivisor(DeltaDivisor)

  implicit object DeltasDivisor extends Divisor[MatchSum.Statistics.Deltas, MatchQuotient.Statistics.Deltas] {
    def divide(sum: MatchSum.Statistics.Deltas): MatchQuotient.Statistics.Deltas = {
      MatchQuotient.Statistics.Deltas(
        csDiff = sum.csDiff.quotient,
        xpDiff = sum.xpDiff.quotient,
        damageTakenDiff = sum.damageTakenDiff.quotient,
        xpPerMin = sum.xpPerMin.quotient,
        goldPerMin = sum.goldPerMin.quotient,
        towersPerMin = sum.towersPerMin.quotient,
        wardsPlaced = sum.wardsPlaced.quotient,
        damageTaken = sum.damageTaken.quotient
      )
    }
  }

  implicit val deltasOptDivisor = optionDivisor(DeltasDivisor)

  implicit object StatisticsDivisor extends Divisor[MatchSum.Statistics, MatchQuotient.Statistics] {
    def divide(sum: MatchSum.Statistics): MatchQuotient.Statistics = {
      MatchQuotient.Statistics(
        plays = sum.plays,
        scalars = sum.scalars.quotient,
        deltas = sum.deltas.quotient
      )
    }
  }

  implicit def subscalarsMapDivisor[T]: CompositeDivisor[Map[T, MatchSum.Collections.Subscalars], Map[T, MatchQuotient.Collections.Subscalars], Int, Double] = {
    new CompositeDivisor[Map[T, MatchSum.Collections.Subscalars], Map[T, MatchQuotient.Collections.Subscalars], Int, Double] {
      override def divide(sum: Map[T, MatchSum.Collections.Subscalars])(implicit ct: Divisor[Int, Double]): Map[T, MatchQuotient.Collections.Subscalars] = {
        sum.mapValues(_.quotientC)
      }
    }
  }

  implicit def collectionDivisor[SumT, QuotT](implicit extractor: SubscalarsMapping[SumT, QuotT]): CompositeDivisor[SumT, QuotT, Int, Double] = {
    new CompositeDivisor[SumT, QuotT, Int, Double] {
      override def divide(sum: SumT)(implicit ct: Divisor[Int, Double]): QuotT = {
        val ss = extractor.extract(sum)
        extractor.build(sum, ss.map(_.quotientC))
      }
    }
  }

  implicit def collectionSeqDivisor[SumT, QuotT](implicit extractor: SubscalarsMapping[SumT, QuotT]): CompositeDivisor[Seq[SumT], Seq[QuotT], Int, Double] = {
    new CompositeDivisor[Seq[SumT], Seq[QuotT], Int, Double] {
      override def divide(sum: Seq[SumT])(implicit ct: Divisor[Int, Double]): Seq[QuotT] = {
        sum.map(_.quotientC)
      }
    }
  }

  implicit object CollectionsDivisor extends CompositeDivisor[MatchSum.Collections, MatchQuotient.Collections, Int, Double] {
    def divide(sum: MatchSum.Collections)(implicit ct: Divisor[Int, Double]): MatchQuotient.Collections = {
      MatchQuotient.Collections(
        masteries = sum.masteries.quotientC,
        runes = sum.runes.quotientC,
        keystones = sum.keystones.quotientC,
        summoners = sum.summoners.quotientC,
        startingTrinkets = sum.startingTrinkets.quotientC,
        endingTrinkets = sum.endingTrinkets.quotientC,
        skillOrders = sum.skillOrders.quotientC,
        durations = sum.durations.quotientC,
        bans = sum.bans.quotientC,
        allies = sum.allies.quotientC,
        enemies = sum.enemies.quotientC,
        starterItems = sum.starterItems.quotientC,
        coreBuilds = sum.coreBuilds.quotientC
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
        collections = sum.collections.quotientC
      )
    }
  }

  /**
    * Allows running .divide on any Divisor.
    */
  implicit class Divisible[SumT](sum: SumT) {
    /**
      * Applies a divisor.
      */
    def quotient[QuotT](implicit divisor: Divisor[SumT, QuotT]): QuotT = divisor.divide(sum)

    /**
      * Uses the composite divisor.
      */
    def quotientC[QuotT, SumT2, QuotT2](
      implicit compositeDivisor: CompositeDivisor[SumT, QuotT, SumT2, QuotT2],
      divisor: Divisor[SumT2, QuotT2]
    ): QuotT = compositeDivisor.divide(sum)

  }

}
