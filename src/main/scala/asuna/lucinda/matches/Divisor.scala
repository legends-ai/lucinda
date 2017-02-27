package asuna.lucinda.matches

import cats.Monoid
import cats.implicits._
import scala.language.implicitConversions
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import asuna.proto.league.MatchSum.{ Collections => SC }
import asuna.proto.league.lucinda.MatchQuotient.{ Collections => QC }
import asuna.common.legends.MatchSumHelpers._


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
case class RealDivisor(divisor: Long) extends Divisor[Double, Double] {

  override def divide(dividend: Double): Double =
    if (divisor != 0) dividend / divisor.toDouble else 0

}

object RealDivisor {

  implicit def numericRealDivisor[T](div: Divisor[Double, Double])(implicit num: Numeric[T]): Divisor[T, Double] = {
    new Divisor[T, Double] {
      override def divide(dividend: T): Double = div.divide(num.toDouble(dividend))
    }
  }

}

case class DurationDistributionDivisor(dd: MatchSum.Deltas.DurationDistribution)
    extends Divisor[MatchSum.Deltas.Delta, MatchQuotient.Deltas.Delta] {

  override def divide(delta: MatchSum.Deltas.Delta): MatchQuotient.Deltas.Delta = {
    // TODO(igm): profile. this is a lot of allocations for no good reason
    MatchQuotient.Deltas.Delta(
      zeroToTen = RealDivisor(dd.zeroToTen).divide(delta.zeroToTen),
      tenToTwenty = RealDivisor(dd.tenToTwenty).divide(delta.tenToTwenty),
      twentyToThirty = RealDivisor(dd.twentyToThirty).divide(delta.twentyToThirty),
      thirtyToEnd = RealDivisor(dd.thirtyToEnd).divide(delta.thirtyToEnd)
    )
  }

}


object Divisor {

  implicit def optionDivisor[SumT, QuotT](implicit div: Divisor[SumT, QuotT]): Divisor[Option[SumT], Option[QuotT]] = {
    new Divisor[Option[SumT], Option[QuotT]] {
      override def divide(sum: Option[SumT]): Option[QuotT] = {
        sum.map(div.divide)
      }
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

  implicit object SubscalarDivisor extends CompositeDivisor[SC.Subscalars, QC.Subscalars, Long, Double] {

    def divide(sum: SC.Subscalars)(implicit countDivisor: Divisor[Long, Double]): QC.Subscalars = {
      QC.Subscalars(
        playRate = sum.plays.quotient,
        winRate = RealDivisor(sum.plays).divide(sum.wins.toDouble),
        playCount = sum.plays.toInt
      )
    }

  }

  implicit object ScalarsDivisor extends CompositeDivisor[MatchSum.Scalars, MatchQuotient.Scalars, Long, Double] {
    def divide(sum: MatchSum.Scalars)(implicit count: Divisor[Long, Double]): MatchQuotient.Scalars = {
      MatchQuotient.Scalars(
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
        structureDamage = sum.structureDamage.quotient,
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

  implicit object DeltasDivisor extends Divisor[MatchSum.Deltas, MatchQuotient.Deltas] {
    def divide(sum: MatchSum.Deltas): MatchQuotient.Deltas = {
      implicit val divisor = DurationDistributionDivisor(
        sum.durationDistribution.getOrElse(MatchSum.Deltas.DurationDistribution()))
      MatchQuotient.Deltas(
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

  implicit def collectionDivisor[SumT, QuotT](implicit extractor: SubscalarMapping[SumT, QuotT]): CompositeDivisor[SumT, QuotT, Long, Double] = {
    new CompositeDivisor[SumT, QuotT, Long, Double] {
      override def divide(sum: SumT)(implicit count: Divisor[Long, Double]): QuotT = {
        val ss = extractor.extract(sum)
        extractor.build(sum, ss.map(_.quotientC))
      }
    }
  }

  implicit def collectionSeqDivisor[SumT, QuotT](implicit extractor: SubscalarMapping[SumT, QuotT]): CompositeDivisor[Seq[SumT], Seq[QuotT], Long, Double] = {
    new CompositeDivisor[Seq[SumT], Seq[QuotT], Long, Double] {
      override def divide(sum: Seq[SumT])(implicit count: Divisor[Long, Double]): Seq[QuotT] = {
        sum.map(_.quotientC)
      }
    }
  }

  implicit def subscalarMapDivisor[T]: CompositeDivisor[Map[T, SC.Subscalars], Map[T, QC.Subscalars], Long, Double] = {
    new CompositeDivisor[Map[T, SC.Subscalars], Map[T, QC.Subscalars], Long, Double] {
      override def divide(sum: Map[T, SC.Subscalars])(implicit count: Divisor[Long, Double]): Map[T, QC.Subscalars] = {
        sum.mapValues(_.quotientC)
      }
    }
  }

  implicit object CollectionsDivisor extends CompositeDivisor[MatchSum.Collections, MatchQuotient.Collections, Long, Double] {
    def divide(sum: MatchSum.Collections)(implicit count: Divisor[Long, Double]): MatchQuotient.Collections = {
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

  implicit object QuotientDivisor extends Divisor[MatchSum, MatchQuotient] {
    def divide(sum: MatchSum): MatchQuotient = {
      implicit val divisor: Divisor[Long, Double] = RealDivisor(sum.scalars.map(_.plays).orEmpty)
      MatchQuotient(
        scalars = sum.scalars.quotientC,
        deltas = sum.deltas.quotient,
        collections = sum.collections.quotientC
      )
    }
  }

  /**
    * Allows running .divide on any Divisor.
    */
  implicit class Divisible[SumT](sum: SumT) {
    /**
      * Uses a normal divisor.
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
