package asuna.lucinda.statistics

import scala.math.Numeric
import scala.math.Numeric.Implicits._
import asuna.proto.league.MatchSum.Statistics.{ Moments => SMoments }
import asuna.proto.league.lucinda.MatchQuotient.Statistics.{ Moments => QMoments }

import asuna.proto.league.lucinda.AllChampionStatistics.{ Quotients, Sums }

object QuotientsGenerator {
  import asuna.common.legends.MomentsHelpers._

  def generateQuotients(sums: Sums): Quotients = {
    Quotients(
      scalars = sums.scalars match {
        case Some(sc) => Some(generateScalarsQuotients(sc))
        case None => Some(Quotients.Scalars())
      },
      deltas = sums.deltas match {
        case Some(de) => Some(generateDeltasQuotients(de))
        case None => Some(Quotients.Deltas())
      }
    )
  }

  def divide(map: Map[Int, SMoments]): Map[Int, QMoments] = {
    map.mapValues(_.toQuotient)
  }

  def generateScalarsQuotients(scalars: Sums.Scalars): Quotients.Scalars = {
    Quotients.Scalars(
      wins = divide(scalars.wins),
      goldEarned = divide(scalars.goldEarned),
      kills = divide(scalars.kills),
      deaths = divide(scalars.deaths),
      assists = divide(scalars.assists),
      damageDealt = divide(scalars.damageDealt),
      damageTaken = divide(scalars.damageTaken),
      minionsKilled = divide(scalars.minionsKilled),
      teamJungleMinionsKilled = divide(scalars.teamJungleMinionsKilled),
      enemyJungleMinionsKilled = divide(scalars.enemyJungleMinionsKilled),
      killingSpree = divide(scalars.killingSpree),
      wardsBought = divide(scalars.wardsBought),
      wardsPlaced = divide(scalars.wardsPlaced),
      wardsKilled = divide(scalars.wardsKilled),
      crowdControl = divide(scalars.crowdControl),
      firstBlood = divide(scalars.firstBlood),
      firstBloodAssist = divide(scalars.firstBloodAssist),
      doublekills = divide(scalars.doublekills),
      triplekills = divide(scalars.triplekills),
      quadrakills = divide(scalars.quadrakills),
      pentakills = divide(scalars.pentakills),
      physicalDamage = divide(scalars.physicalDamage),
      magicDamage = divide(scalars.magicDamage),
      trueDamage = divide(scalars.trueDamage),
      baronsEncountered = divide(scalars.baronsEncountered),
      baronsKilled = divide(scalars.baronsKilled),
      dragonsEncountered = divideDragons(scalars.dragonsEncountered),
      dragonsKilled = divideDragons(scalars.dragonsKilled)
    )
  }

  def divideDragons(sum: Seq[Sums.Scalars.DragonStat]): Seq[Quotients.Scalars.DragonStat] = {
    sum.map { stat =>
      Quotients.Scalars.DragonStat(
        dragon = stat.dragon,
        value = divide(stat.value)
      )
    }
  }

  def generateDeltasQuotients(deltas: Sums.Deltas): Quotients.Deltas = {
    Quotients.Deltas(
      csDiff = divideDeltasOption(deltas.csDiff),
      xpDiff = divideDeltasOption(deltas.xpDiff),
      damageTakenDiff = divideDeltasOption(deltas.damageTakenDiff),
      xpPerMin = divideDeltasOption(deltas.xpPerMin),
      goldPerMin = divideDeltasOption(deltas.goldPerMin),
      towersPerMin = divideDeltasOption(deltas.towersPerMin),
      wardsPlaced = divideDeltasOption(deltas.wardsPlaced),
      damageTaken = divideDeltasOption(deltas.damageTaken)
    )
  }

  def divideDeltasOption(delta: Option[Sums.Deltas.Delta]): Option[Quotients.Deltas.Delta] = {
    delta match {
      case Some(d) => Some(divideDeltas(d))
      case None => None
    }
  }

  /**
    * Generates the quotient from the delta sums and duration distributions.
    */
  def divideDeltas(delta: Sums.Deltas.Delta): Quotients.Deltas.Delta = {
    Quotients.Deltas.Delta(
      zeroToTen = divide(delta.zeroToTen),
      tenToTwenty = divide(delta.tenToTwenty),
      twentyToThirty = divide(delta.twentyToThirty),
      thirtyToEnd = divide(delta.thirtyToEnd)
    )
  }

  def divideScalars[S: Numeric, T: Numeric](divisors: Map[Int, S], scalars: Map[Int, T]): Map[Int, Double] = {
    scalars.transform((k, v) =>
      divisors.mapValues(_.toDouble).get(k) match {
        case Some(divisor) if divisor != 0 => v.toDouble / divisor
        case _ => 0
      }
    )
  }

}
