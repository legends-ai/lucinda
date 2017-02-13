package asuna.lucinda.statistics

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.AllChampionStatistics.Sums
import cats.implicits._

object SumConversionHelpers {

  implicit class ScalarsConversion(scalars: MatchSum.Scalars) {

    def asAggregate(champion: Int): Sums.Scalars = {
      Sums.Scalars(
        plays = Map(champion -> scalars.plays),
        wins = Map(champion -> scalars.wins),
        goldEarned = Map(champion -> scalars.goldEarned),
        kills = Map(champion -> scalars.kills),
        deaths = Map(champion -> scalars.deaths),
        assists = Map(champion -> scalars.assists),
        damageDealt = Map(champion -> scalars.damageDealt),
        damageTaken = Map(champion -> scalars.damageTaken),
        minionsKilled = Map(champion -> scalars.minionsKilled),
        teamJungleMinionsKilled = Map(champion -> scalars.teamJungleMinionsKilled),
        enemyJungleMinionsKilled = Map(champion -> scalars.enemyJungleMinionsKilled),
        structureDamage = Map(champion -> scalars.structureDamage),
        killingSpree = Map(champion -> scalars.killingSpree),
        wardsBought = Map(champion -> scalars.wardsBought),
        wardsPlaced = Map(champion -> scalars.wardsPlaced),
        wardsKilled = Map(champion -> scalars.wardsKilled),
        crowdControl = Map(champion -> scalars.crowdControl),
        firstBlood = Map(champion -> scalars.firstBlood),
        firstBloodAssist = Map(champion -> scalars.firstBloodAssist),
        doublekills = Map(champion -> scalars.doublekills),
        triplekills = Map(champion -> scalars.triplekills),
        quadrakills = Map(champion -> scalars.quadrakills),
        pentakills = Map(champion -> scalars.pentakills),
        physicalDamage = Map(champion -> scalars.physicalDamage),
        magicDamage = Map(champion -> scalars.magicDamage),
        trueDamage = Map(champion -> scalars.trueDamage)
      )
    }

  }

  implicit class DeltaConversion(delta: MatchSum.Deltas.Delta) {

    def asAggregate(champion: Int): Sums.Deltas.Delta = {
      Sums.Deltas.Delta(
        zeroToTen = Map(champion -> delta.zeroToTen),
        tenToTwenty = Map(champion -> delta.tenToTwenty),
        twentyToThirty = Map(champion -> delta.twentyToThirty),
        thirtyToEnd = Map(champion -> delta.thirtyToEnd)
      )
    }

  }

  implicit class DeltasConversion(deltas: MatchSum.Deltas) {

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

  implicit class DurationDistributionsConversion(dd: MatchSum.Deltas.DurationDistribution) {

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
        scalars = Some(sum.scalars.orEmpty.asAggregate(champion)),
        deltas = Some(sum.deltas.orEmpty.asAggregate(champion)),
        durationDistributions = Some(
          sum.deltas.flatMap(_.durationDistribution).orEmpty.asAggregate(champion)
        ),
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
