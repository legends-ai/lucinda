package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.{Results, Quotients}

/**
  * Generats the Results part of the statistics.
  */
object ResultsGenerator {

  def generateResults(quotients: Quotients): Results = {
    Results(
      scalars = Some(makeScalars(quotients.scalars.getOrElse(Quotients.Scalars())))
    )
  }

  def makeScalars(scalars: Quotients.Scalars): Results.Scalars = {
    Results.Scalars(
      wins = makeStat(scalars.wins),
      goldEarned = makeStat(scalars.goldEarned),
      kills = makeStat(scalars.kills),
      deaths = makeStat(scalars.deaths),
      assists = makeStat(scalars.assists),
      damageDealt = makeStat(scalars.damageDealt),
      damageTaken = makeStat(scalars.damageTaken),
      minionsKilled = makeStat(scalars.minionsKilled),
      teamJungleMinionsKilled = makeStat(scalars.teamJungleMinionsKilled),
      enemyJungleMinionsKilled = makeStat(scalars.enemyJungleMinionsKilled),
      structureDamage = makeStat(scalars.structureDamage),
      killingSpree = makeStat(scalars.killingSpree),
      wardsBought = makeStat(scalars.wardsBought),
      wardsPlaced = makeStat(scalars.wardsPlaced),
      wardsKilled = makeStat(scalars.wardsKilled),
      crowdControl = makeStat(scalars.crowdControl),
      firstBlood = makeStat(scalars.firstBlood),
      firstBloodAssist = makeStat(scalars.firstBloodAssist),
      doublekills = makeStat(scalars.doublekills),
      triplekills = makeStat(scalars.triplekills),
      quadrakills = makeStat(scalars.quadrakills),
      pentakills = makeStat(scalars.pentakills),
      physicalDamage = makeStat(scalars.physicalDamage),
      magicDamage = makeStat(scalars.magicDamage),
      trueDamage = makeStat(scalars.trueDamage)
    )
  }

  def makeStat(statsMap: Map[Int, Double]): Map[Int, Results.Statistic] = {
    val sortedPairs = statsMap.toSeq.sortBy(_._2)

    // average of the value
    val average =  statsMap.size match {
      case 0 => 0
      case _ => statsMap.values.sum / statsMap.values.size
    }

    val statsWithIndex = sortedPairs.zipWithIndex.map { case ((champ, value), index) =>
      (champ, (value, index))
    }.toMap

    statsWithIndex.mapValues { case (value, index) =>
      Results.Statistic(
        rank = index + 1,
        value = value,
        average = average,
        percentile = 1 - index / statsMap.size
      )
    }
  }

}
