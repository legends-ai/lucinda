package asuna.lucinda.statistics

import asuna.proto.league.Role
import asuna.proto.league.lucinda.AllChampionStatistics
import cats.Monoid
import cats.implicits._

object SumsHelpers {

  implicit object ScalarsMonoid extends Monoid[AllChampionStatistics.Sums.Scalars] {

    def combine(a: AllChampionStatistics.Sums.Scalars, b: AllChampionStatistics.Sums.Scalars): AllChampionStatistics.Sums.Scalars = {
      AllChampionStatistics.Sums.Scalars(
        plays = a.plays |+| b.plays,
        wins = a.wins |+| b.wins,
        goldEarned = a.goldEarned |+| b.goldEarned,
        kills = a.kills |+| b.kills,
        deaths = a.deaths |+| b.deaths,
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

    def empty = AllChampionStatistics.Sums.Scalars()
  }

  implicit object DeltaMonoid extends Monoid[AllChampionStatistics.Sums.Deltas.Delta] {

    def combine(a: AllChampionStatistics.Sums.Deltas.Delta, b: AllChampionStatistics.Sums.Deltas.Delta): AllChampionStatistics.Sums.Deltas.Delta = {
      AllChampionStatistics.Sums.Deltas.Delta(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def empty = AllChampionStatistics.Sums.Deltas.Delta()
  }

  implicit object DeltasMonoid extends Monoid[AllChampionStatistics.Sums.Deltas] {

    def combine(a: AllChampionStatistics.Sums.Deltas, b: AllChampionStatistics.Sums.Deltas): AllChampionStatistics.Sums.Deltas = {
      AllChampionStatistics.Sums.Deltas(
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

    def empty = AllChampionStatistics.Sums.Deltas(
      csDiff = Some(DeltaMonoid.empty),
      xpDiff = Some(DeltaMonoid.empty),
      damageTakenDiff = Some(DeltaMonoid.empty),
      xpPerMin = Some(DeltaMonoid.empty),
      goldPerMin = Some(DeltaMonoid.empty),
      towersPerMin = Some(DeltaMonoid.empty),
      wardsPlaced = Some(DeltaMonoid.empty),
      damageTaken = Some(DeltaMonoid.empty)
    )
  }

  implicit object DurationDistributionsMonoid extends Monoid[AllChampionStatistics.Sums.DurationDistributions] {

    def combine(
      a: AllChampionStatistics.Sums.DurationDistributions,
      b: AllChampionStatistics.Sums.DurationDistributions): AllChampionStatistics.Sums.DurationDistributions = {
      AllChampionStatistics.Sums.DurationDistributions(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def empty = AllChampionStatistics.Sums.DurationDistributions()
  }

  implicit object SubscalarMonoid extends Monoid[AllChampionStatistics.Sums.Subscalars.Subscalar] {

    def combine(
      a: AllChampionStatistics.Sums.Subscalars.Subscalar,
      b: AllChampionStatistics.Sums.Subscalars.Subscalar) : AllChampionStatistics.Sums.Subscalars.Subscalar = {
      AllChampionStatistics.Sums.Subscalars.Subscalar(
        plays = a.plays |+| b.plays,
        wins = a.wins |+| b.wins
      )
    }

    def empty = AllChampionStatistics.Sums.Subscalars.Subscalar()
  }

  implicit object SubscalarsMonoid extends Monoid[AllChampionStatistics.Sums.Subscalars] {

    def combine(
      a: AllChampionStatistics.Sums.Subscalars,
      b: AllChampionStatistics.Sums.Subscalars): AllChampionStatistics.Sums.Subscalars = {
      AllChampionStatistics.Sums.Subscalars(
        bans = a.bans |+| b.bans,
        allies = a.allies |+| b.allies
      )
    }

    def empty = AllChampionStatistics.Sums.Subscalars()
  }

  implicit object SumsMonoid extends Monoid[AllChampionStatistics.Sums] {

    def combine(a: AllChampionStatistics.Sums, b: AllChampionStatistics.Sums): AllChampionStatistics.Sums = {
      // TODO(igm): hard part
      AllChampionStatistics.Sums(
        scalars = a.scalars |+| b.scalars,
        deltas = a.deltas |+| b.deltas,
        durationDistributions = a.durationDistributions |+| b.durationDistributions,
        subscalars = a.subscalars |+| b.subscalars
      )
    }

    def empty = AllChampionStatistics.Sums(
      scalars = Some(ScalarsMonoid.empty),
      deltas = Some(DeltasMonoid.empty),
      durationDistributions = Some(DurationDistributionsMonoid.empty),
      subscalars = Some(SubscalarsMonoid.empty)
    )
  }

}
