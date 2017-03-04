package asuna.lucinda.statistics

import asuna.common.legends.MatchSumHelpers._
import asuna.common.legends.MomentsHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.AllChampionStatistics.Sums
import cats.implicits._

object SumConversionHelpers {

  implicit class ScalarsConversion(scalars: MatchSum.Statistics.Scalars) {

    def asAggregate(champion: Int, plays: Int): Sums.Scalars = {
      Sums.Scalars(
        plays = Map(champion -> plays),
        wins = Map(champion -> scalars.wins.map(_.sum).orEmpty.toLong),
        goldEarned = Map(champion -> scalars.goldEarned.map(_.sum).orEmpty.toLong),
        kills = Map(champion -> scalars.kills.map(_.sum).orEmpty.toLong),
        deaths = Map(champion -> scalars.deaths.map(_.sum).orEmpty.toLong),
        assists = Map(champion -> scalars.assists.map(_.sum).orEmpty.toLong),
        damageDealt = Map(champion -> scalars.damageDealt.map(_.sum).orEmpty.toLong),
        damageTaken = Map(champion -> scalars.damageTaken.map(_.sum).orEmpty.toLong),
        minionsKilled = Map(champion -> scalars.minionsKilled.map(_.sum).orEmpty.toLong),
        teamJungleMinionsKilled = Map(champion -> scalars.teamJungleMinionsKilled.map(_.sum).orEmpty.toLong),
        enemyJungleMinionsKilled = Map(champion -> scalars.enemyJungleMinionsKilled.map(_.sum).orEmpty.toLong),
        killingSpree = Map(champion -> scalars.killingSpree.map(_.sum).orEmpty.toLong),
        wardsBought = Map(champion -> scalars.wardsBought.map(_.sum).orEmpty.toLong),
        wardsPlaced = Map(champion -> scalars.wardsPlaced.map(_.sum).orEmpty.toLong),
        wardsKilled = Map(champion -> scalars.wardsKilled.map(_.sum).orEmpty.toLong),
        crowdControl = Map(champion -> scalars.crowdControl.map(_.sum).orEmpty.toLong),
        firstBlood = Map(champion -> scalars.firstBlood.map(_.sum).orEmpty.toLong),
        firstBloodAssist = Map(champion -> scalars.firstBloodAssist.map(_.sum).orEmpty.toLong),
        doublekills = Map(champion -> scalars.doublekills.map(_.sum).orEmpty.toLong),
        triplekills = Map(champion -> scalars.triplekills.map(_.sum).orEmpty.toLong),
        quadrakills = Map(champion -> scalars.quadrakills.map(_.sum).orEmpty.toLong),
        pentakills = Map(champion -> scalars.pentakills.map(_.sum).orEmpty.toLong),
        physicalDamage = Map(champion -> scalars.physicalDamage.map(_.sum).orEmpty.toLong),
        magicDamage = Map(champion -> scalars.magicDamage.map(_.sum).orEmpty.toLong),
        trueDamage = Map(champion -> scalars.trueDamage.map(_.sum).orEmpty.toLong)
      )
    }

  }

  implicit class DeltaConversion(delta: MatchSum.Statistics.Deltas.Delta) {

    def asAggregate(champion: Int): Sums.Deltas.Delta = {
      Sums.Deltas.Delta(
        zeroToTen = Map(champion -> delta.zeroToTen.map(_.sum).orEmpty.toLong),
        tenToTwenty = Map(champion -> delta.tenToTwenty.map(_.sum).orEmpty.toLong),
        twentyToThirty = Map(champion -> delta.twentyToThirty.map(_.sum).orEmpty.toLong),
        thirtyToEnd = Map(champion -> delta.thirtyToEnd.map(_.sum).orEmpty.toLong)
      )
    }

  }

  implicit class DeltasConversion(deltas: MatchSum.Statistics.Deltas) {

    def asAggregate(champion: Int): Sums.Deltas = {
      Sums.Deltas(
        csDiff = Some(deltas.csDiff.orEmpty.asAggregate(champion)),
        xpDiff = Some(deltas.xpDiff.orEmpty.asAggregate(champion)),
        damageTakenDiff = Some(deltas.damageTakenDiff.orEmpty.asAggregate(champion)),
        xpPerMin = Some(deltas.xpPerMin.orEmpty.asAggregate(champion)),
        goldPerMin = Some(deltas.goldPerMin.orEmpty.asAggregate(champion)),
        towersPerMin = Some(deltas.towersPerMin.orEmpty.asAggregate(champion)),
        wardsPlaced = Some(deltas.wardsPlaced.orEmpty.asAggregate(champion)),
        damageTaken = Some(deltas.damageTaken.orEmpty.asAggregate(champion))
      )
    }

  }

  implicit class DurationDistributionsConversion(dd: MatchSum.Statistics.Deltas.DurationDistribution) {

    def asAggregate(champion: Int): Sums.DurationDistributions = {
      Sums.DurationDistributions(
        zeroToTen = Map(champion -> dd.zeroToTen),
        tenToTwenty = Map(champion -> dd.tenToTwenty),
        twentyToThirty = Map(champion -> dd.twentyToThirty),
        thirtyToEnd = Map(champion -> dd.thirtyToEnd)
      )
    }

  }

  implicit class SubscalarsMapConversion(ss: Map[Int, MatchSum.Collections.Subscalars]) {

    def asAggregate(champion: Int): Map[Int, Sums.Subscalars.Subscalar] = {
      ss.mapValues { indiv =>
        Sums.Subscalars.Subscalar(
          plays = Map(champion -> indiv.plays),
          wins = Map(champion -> indiv.wins)
        )
      }
    }

  }

  implicit class MatchSumConversion(sum: MatchSum) {

    def asAggregate(champion: Int): Sums = {
      Sums(
        scalars = sum.statistics
          .flatMap(_.scalars).map(_.asAggregate(champion, sum.statistics.map(_.plays).orEmpty)),
        deltas = sum.statistics
          .flatMap(_.deltas).map(_.asAggregate(champion)),
        durationDistributions = sum.statistics.flatMap(_.deltas)
          .flatMap(_.durationDistribution).map(_.asAggregate(champion)),
        subscalars = Some(asSubscalarsAggregate(champion))
      )
    }

    def asSubscalarsAggregate(champion: Int): Sums.Subscalars = {
      Sums.Subscalars(
        bans = sum.collections.map(_.bans)
          .getOrElse(Map()).asAggregate(champion),
        allies = sum.collections.map(_.allies)
          .getOrElse(Map()).asAggregate(champion)
      )
    }

  }

}
