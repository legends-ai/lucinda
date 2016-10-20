package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.{Quotients, Sums}

object QuotientsGenerator {

  def generateQuotients(sums: Sums): Quotients = {
    Quotients(
      scalars = sums.scalars match {
        case Some(sc) => Some(generateScalarsQuotients(sc))
        case None => None
      },
      deltas = sums.deltas match {
        case Some(de) => Some(generateDeltasQuotients(de))
        case None => None
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

  def generateDeltasQuotients(deltas: Sums.Deltas): Quotients.Deltas = {
    Quotients.Deltas()
  }

  def divideScalars(divisor: Map[Int, Long], scalars: Map[Int, Long]): Map[Int, Double] = {
    val divisorDoubles = divisor.mapValues(_.toDouble)
    scalars.transform((k, v) =>
      divisorDoubles.get(k) match {
        case Some(divisor) if divisor != 0 => v.toDouble / divisor
        case None => 0
      }
    )
  }

}
