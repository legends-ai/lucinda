package asuna.lucinda.statistics

import asuna.proto.league.lucinda.Statistic
import asuna.proto.league.lucinda.MatchQuotient.Statistics.Moments
import asuna.proto.league.lucinda.AllChampionStatistics.{ Results, Sums, Quotients }
import cats.implicits._

/**
  * Generats the Results part of the statistics.
  */
case class ResultsGenerator(sums: Sums, quotients: Quotients) {

  def generate(): Results = {
    Results(
      scalars = quotients.scalars.map(makeScalars),
      deltas = quotients.deltas.map(d =>
        makeDeltas(d, sums.durationDistributions.getOrElse(Sums.DurationDistributions()))),
      derivatives = Some(derivatives)
    )
  }

  def makeScalars(scalars: Quotients.Scalars): Results.Scalars = {
    Results.Scalars(
      wins = makeStat(scalars.wins, scalars.plays),
      goldEarned = makeStat(scalars.goldEarned, scalars.plays),
      kills = makeStat(scalars.kills, scalars.plays),
      deaths = makeStat(scalars.deaths, scalars.plays),
      assists = makeStat(scalars.assists, scalars.plays),
      damageDealt = makeStat(scalars.damageDealt, scalars.plays),
      damageTaken = makeStat(scalars.damageTaken, scalars.plays),
      minionsKilled = makeStat(scalars.minionsKilled, scalars.plays),
      teamJungleMinionsKilled = makeStat(scalars.teamJungleMinionsKilled, scalars.plays),
      enemyJungleMinionsKilled = makeStat(scalars.enemyJungleMinionsKilled, scalars.plays),
      structureDamage = makeStat(scalars.structureDamage, scalars.plays),
      killingSpree = makeStat(scalars.killingSpree, scalars.plays),
      wardsBought = makeStat(scalars.wardsBought, scalars.plays),
      wardsPlaced = makeStat(scalars.wardsPlaced, scalars.plays),
      wardsKilled = makeStat(scalars.wardsKilled, scalars.plays),
      crowdControl = makeStat(scalars.crowdControl, scalars.plays),
      firstBlood = makeStat(scalars.firstBlood, scalars.plays),
      firstBloodAssist = makeStat(scalars.firstBloodAssist, scalars.plays),
      doublekills = makeStat(scalars.doublekills, scalars.plays),
      triplekills = makeStat(scalars.triplekills, scalars.plays),
      quadrakills = makeStat(scalars.quadrakills, scalars.plays),
      pentakills = makeStat(scalars.pentakills, scalars.plays),

      physicalDamage = makeStat(scalars.physicalDamage, scalars.plays),
      magicDamage = makeStat(scalars.magicDamage, scalars.plays),
      trueDamage = makeStat(scalars.trueDamage, scalars.plays)
    )
  }

  def makeDeltas(deltas: Quotients.Deltas, dds: Sums.DurationDistributions): Results.Deltas = {
    Results.Deltas(
      csDiff = makeDeltaOption(deltas.csDiff, dds),
      xpDiff = makeDeltaOption(deltas.xpDiff, dds),
      damageTakenDiff = makeDeltaOption(deltas.damageTakenDiff, dds),
      xpPerMin = makeDeltaOption(deltas.xpPerMin, dds),
      goldPerMin = makeDeltaOption(deltas.goldPerMin, dds),
      towersPerMin = makeDeltaOption(deltas.towersPerMin, dds),
      wardsPlaced = makeDeltaOption(deltas.wardsPlaced, dds),
      damageTaken = makeDeltaOption(deltas.damageTaken, dds)
    )
  }

  def makeDeltaOption(delta: Option[Quotients.Deltas.Delta], dds: Sums.DurationDistributions): Option[Results.Deltas.Delta] = {
    delta.map(d => makeDelta(d, dds))
  }

  def makeDelta(delta: Quotients.Deltas.Delta, dds: Sums.DurationDistributions): Results.Deltas.Delta = {
    Results.Deltas.Delta(
      zeroToTen = makeStat(delta.zeroToTen, dds.zeroToTen),
      tenToTwenty = makeStat(delta.tenToTwenty, dds.tenToTwenty),
      twentyToThirty = makeStat(delta.twentyToThirty, dds.twentyToThirty),
      thirtyToEnd = makeStat(delta.thirtyToEnd, dds.thirtyToEnd)
    )
  }

  def makeStat(statsMap: Map[Int, Double], sumsMap: Map[Int, Long]): Map[Int, Statistic] = {
    val sortedPairs = statsMap.toSeq.sortBy(_._2).reverse

    // Map of champ to total of the stat. Used for computing mean across role.
    val pairsMap = statsMap.transform { case (k, v) =>
      v * sumsMap.get(k).orEmpty
    }

    // average of the value
    val average =  statsMap.size match {
      case 0 => 0
      case _ => pairsMap.values.sum / pairsMap.values.size
    }

    val statsWithIndex = sortedPairs.zipWithIndex.map { case ((champ, value), index) =>
      (champ, (value, index))
    }.toMap

    statsWithIndex.mapValues { case (value, index) =>
      Statistic(
        rank = index + 1,
        mean = value,
        meanAcrossRole = average,
        // TODO(igm): is this what we mean by percentile?
        percentile = 1 - index.toDouble / statsMap.size
      )
    }
  }

  def derivatives: Results.Derivatives = {
    val counts = sums.scalars.map(_.plays).orEmpty
    val pickRateMap = for {
      plays <- sums.scalars.map(_.plays)
    } yield {
      // total number of plays across all champions
      val total = plays.values.sum
      // total number of games. 10 since 10 champs per game.
      // TODO(igm): tweak based off game mode. twisted treeline?
      val totalGames = total / 10
      plays.mapValues(_.toDouble / totalGames)
    }
    val banRateMap = for {
      banCount <- sums.subscalars.map(_.bans.mapValues(_.plays))
    } yield {
      val bans = banCount.mapValues(_.values.toList.combineAll)
      // 6 bans per match. This finds us the total number of matches.
      val total = bans.values.sum.toDouble / 6
      bans.mapValues(_.toDouble / total)
    }
    Results.Derivatives(
      picks = makeStat(pickRateMap.orEmpty, counts),
      bans = makeStat(banRateMap.orEmpty, counts)
    )
  }

}
