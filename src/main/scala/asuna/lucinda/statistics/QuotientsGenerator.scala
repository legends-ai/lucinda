package asuna.lucinda.statistics

import scala.math.Numeric
import scala.math.Numeric.Implicits._

import asuna.proto.league.lucinda.ChampionStatistics.{ Quotients, Sums }

object QuotientsGenerator {

  def generateQuotients(sums: Sums): Quotients = {
    Quotients(
      scalars = sums.scalars match {
        case Some(sc) => Some(generateScalarsQuotients(sc))
        case None => Some(Quotients.Scalars())
      },
      deltas = sums.deltas match {
        case Some(de) => Some(generateDeltasQuotients(sums.durationDistributions.getOrElse(Sums.DurationDistributions()), de))
        case None => Some(Quotients.Deltas())
      }
    )
  }

  def generateScalarsQuotients(scalars: Sums.Scalars): Quotients.Scalars = {
    val divide = divideScalars(scalars.plays, _: Map[Int, Long])
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
      structureDamage = divide(scalars.structureDamage),
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
      trueDamage = divide(scalars.trueDamage)
    )
  }

  def generateDeltasQuotients(durationDistributions: Sums.DurationDistributions, deltas: Sums.Deltas): Quotients.Deltas = {
    val divide = divideDeltasOption(durationDistributions, _: Option[Sums.Deltas.Delta])
    Quotients.Deltas(
      csDiff = divide(deltas.csDiff),
      xpDiff = divide(deltas.xpDiff),
      damageTakenDiff = divide(deltas.damageTakenDiff),
      xpPerMin = divide(deltas.xpPerMin),
      goldPerMin = divide(deltas.goldPerMin),
      towersPerMin = divide(deltas.towersPerMin),
      wardsPlaced = divide(deltas.wardsPlaced),
      damageTaken = divide(deltas.damageTaken)
    )
  }

  def divideDeltasOption(
    durationDistributions: Sums.DurationDistributions, delta: Option[Sums.Deltas.Delta]
  ): Option[Quotients.Deltas.Delta] = {
    delta match {
      case Some(d) => Some(divideDeltas(durationDistributions, d))
      case None => None
    }
  }

  /**
    * Generates the quotient from the delta sums and duration distributions.
    */
  def divideDeltas(durationDistributions: Sums.DurationDistributions, delta: Sums.Deltas.Delta): Quotients.Deltas.Delta = {
    Quotients.Deltas.Delta(
      zeroToTen = divideScalars(durationDistributions.zeroToTen, delta.zeroToTen),
      tenToTwenty = divideScalars(durationDistributions.tenToTwenty, delta.tenToTwenty),
      twentyToThirty = divideScalars(durationDistributions.twentyToThirty, delta.twentyToThirty),
      thirtyToEnd = divideScalars(durationDistributions.thirtyToEnd, delta.thirtyToEnd)
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
