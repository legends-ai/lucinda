package io.asuna.lucinda.statistics

import scalaz.outlaws.std.double._
import scalaz.Monoid
import scalaz.Scalaz._
import io.asuna.proto.enums.Role
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics

trait StatisticsMonoids {

  implicit object ScalarsMonoid extends Monoid[ChampionStatistics.Sums.Scalars] {

    def append(a: ChampionStatistics.Sums.Scalars, b: => ChampionStatistics.Sums.Scalars): ChampionStatistics.Sums.Scalars = {
      ChampionStatistics.Sums.Scalars(
        plays = a.plays |+| b.plays,
        wins = a.wins |+| b.wins,
        goldEarned = a.goldEarned |+| b.goldEarned,
        kills = a.kills |+| b.kills,
        deaths = a.deaths |+| b.kills,
        assists = a.assists |+| b.assists,
        damageDealt = a.damageDealt |+| b.damageDealt,
        damageTaken = a.damageTaken |+| b.damageTaken,
        minionsKilled = a.minionsKilled |+| b.minionsKilled,
        teamJungleMinionsKilled = a.teamJungleMinionsKilled |+| b.teamJungleMinionsKilled,
        enemyJungleMinionsKilled = a.enemyJungleMinionsKilled |+| b.enemyJungleMinionsKilled,
        structureDamage = a.structureDamage |+| b.structureDamage,
        killingSpree = a.killingSpree |+| b.killingSpree,
        wardsBought = a.wardsBought |+| b.wardsBought,
        wardsPlaced = a.wardsPlaced |+| b.wardsPlaced,
        wardsKilled = a.wardsKilled |+| b.wardsKilled,
        crowdControl = a.crowdControl |+| b.crowdControl,
        firstBlood = a.firstBlood |+| b.firstBlood,
        firstBloodAssist = a.firstBloodAssist |+| b.firstBloodAssist,
        doublekills = a.doublekills |+| b.doublekills,
        triplekills = a.triplekills |+| b.triplekills,
        quadrakills = a.quadrakills |+| b.quadrakills,
        pentakills = a.pentakills |+| b.pentakills,
        physicalDamage = a.physicalDamage |+| b.physicalDamage,
        magicDamage = a.magicDamage |+| b.magicDamage,
        trueDamage = a.trueDamage |+| b.trueDamage
      )
    }

    def zero = ChampionStatistics.Sums.Scalars()
  }

  implicit object DeltaMonoid extends Monoid[ChampionStatistics.Sums.Deltas.Delta] {

    def append(a: ChampionStatistics.Sums.Deltas.Delta, b: => ChampionStatistics.Sums.Deltas.Delta): ChampionStatistics.Sums.Deltas.Delta = {
      ChampionStatistics.Sums.Deltas.Delta(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def zero = ChampionStatistics.Sums.Deltas.Delta()
  }

  implicit object DeltasMonoid extends Monoid[ChampionStatistics.Sums.Deltas] {

    def append(a: ChampionStatistics.Sums.Deltas, b: => ChampionStatistics.Sums.Deltas): ChampionStatistics.Sums.Deltas = {
      ChampionStatistics.Sums.Deltas(
        csDiff = a.csDiff |+| b.csDiff,
        xpDiff = a.xpDiff |+| b.xpDiff,
        damageTakenDiff = a.damageTakenDiff |+| b.damageTakenDiff,
        xpPerMin = a.xpPerMin |+| b.xpPerMin,
        goldPerMin = a.goldPerMin |+| b.goldPerMin,
        towersPerMin = a.towersPerMin |+| b.towersPerMin,
        wardsPlaced = a.wardsPlaced |+| b.wardsPlaced,
        damageTaken = a.damageTaken |+| b.damageTaken
      )
    }

    def zero = ChampionStatistics.Sums.Deltas()
  }

  implicit object DurationDistributionsMonoid extends Monoid[ChampionStatistics.Sums.DurationDistributions] {

    def append(
      a: ChampionStatistics.Sums.DurationDistributions,
      b: => ChampionStatistics.Sums.DurationDistributions): ChampionStatistics.Sums.DurationDistributions = {
      ChampionStatistics.Sums.DurationDistributions(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def zero = ChampionStatistics.Sums.DurationDistributions()
  }

  implicit object SubscalarMonoid extends Monoid[ChampionStatistics.Sums.Subscalars.Subscalar] {

    def append(
      a: ChampionStatistics.Sums.Subscalars.Subscalar,
      b: => ChampionStatistics.Sums.Subscalars.Subscalar) : ChampionStatistics.Sums.Subscalars.Subscalar = {
      ChampionStatistics.Sums.Subscalars.Subscalar(
        plays = a.plays |+| b.plays,
        wins = a.wins |+| b.wins
      )
    }

    def zero = ChampionStatistics.Sums.Subscalars.Subscalar()
  }

  implicit object SubscalarsMonoid extends Monoid[ChampionStatistics.Sums.Subscalars] {

    def append(
      a: ChampionStatistics.Sums.Subscalars,
      b: => ChampionStatistics.Sums.Subscalars): ChampionStatistics.Sums.Subscalars = {
      ChampionStatistics.Sums.Subscalars(
        bans = a.bans |+| b.bans,
        allies = a.allies |+| b.allies
      )
    }

    def zero = ChampionStatistics.Sums.Subscalars()
  }

  implicit object SumsMonoid extends Monoid[ChampionStatistics.Sums] {

    def append(a: ChampionStatistics.Sums, b: => ChampionStatistics.Sums): ChampionStatistics.Sums = {
      // TODO(igm): hard part
      ChampionStatistics.Sums(
        scalars = a.scalars |+| b.scalars,
        deltas = a.deltas |+| b.deltas,
        durationDistributions = a.durationDistributions |+| b.durationDistributions,
        subscalars = a.subscalars |+| b.subscalars
      )
    }

    def zero = ChampionStatistics.Sums()
  }

  implicit object StatisticsMonoid extends Monoid[ChampionStatistics] {

    def append(a: ChampionStatistics, b: ChampionStatistics): ChampionStatistics = {
      // wow a monoid can calculate things too!
      val sums = a.sums |+| b.sums
      val quotients = sums.map(QuotientsGenerator.generateQuotients)
      val results = quotients.map(ResultsGenerator.generateResults)
      ChampionStatistics(
        // Roles should be the same. If they're not, fuck my ass.
        role = a.role,
        results = results,
        quotients = quotients,
        sums = sums
      )
    }

    def zero = ChampionStatistics()

  }

}

object StatisticsCombiner extends StatisticsMonoids {

  def combine(aList: List[ChampionStatistics]): ChampionStatistics = {
    aList.suml
  }

}
