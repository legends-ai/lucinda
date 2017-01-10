package asuna.lucinda.statistics

import asuna.proto.enums.Role
import asuna.proto.lucinda.LucindaData.ChampionStatistics
import cats.Monoid
import cats.implicits._

object SumsHelpers {

  implicit object ScalarsMonoid extends Monoid[ChampionStatistics.Sums.Scalars] {

    def combine(a: ChampionStatistics.Sums.Scalars, b: ChampionStatistics.Sums.Scalars): ChampionStatistics.Sums.Scalars = {
      ChampionStatistics.Sums.Scalars(
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

    def empty = ChampionStatistics.Sums.Scalars()
  }

  implicit object DeltaMonoid extends Monoid[ChampionStatistics.Sums.Deltas.Delta] {

    def combine(a: ChampionStatistics.Sums.Deltas.Delta, b: ChampionStatistics.Sums.Deltas.Delta): ChampionStatistics.Sums.Deltas.Delta = {
      ChampionStatistics.Sums.Deltas.Delta(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def empty = ChampionStatistics.Sums.Deltas.Delta()
  }

  implicit object DeltasMonoid extends Monoid[ChampionStatistics.Sums.Deltas] {

    def combine(a: ChampionStatistics.Sums.Deltas, b: ChampionStatistics.Sums.Deltas): ChampionStatistics.Sums.Deltas = {
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

    def empty = ChampionStatistics.Sums.Deltas(
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

  implicit object DurationDistributionsMonoid extends Monoid[ChampionStatistics.Sums.DurationDistributions] {

    def combine(
      a: ChampionStatistics.Sums.DurationDistributions,
      b: ChampionStatistics.Sums.DurationDistributions): ChampionStatistics.Sums.DurationDistributions = {
      ChampionStatistics.Sums.DurationDistributions(
        zeroToTen = a.zeroToTen |+| b.zeroToTen,
        tenToTwenty = a.tenToTwenty |+| b.tenToTwenty,
        twentyToThirty = a.twentyToThirty |+| b.twentyToThirty,
        thirtyToEnd = a.thirtyToEnd |+| b.thirtyToEnd
      )
    }

    def empty = ChampionStatistics.Sums.DurationDistributions()
  }

  implicit object SubscalarMonoid extends Monoid[ChampionStatistics.Sums.Subscalars.Subscalar] {

    def combine(
      a: ChampionStatistics.Sums.Subscalars.Subscalar,
      b: ChampionStatistics.Sums.Subscalars.Subscalar) : ChampionStatistics.Sums.Subscalars.Subscalar = {
      ChampionStatistics.Sums.Subscalars.Subscalar(
        plays = a.plays |+| b.plays,
        wins = a.wins |+| b.wins
      )
    }

    def empty = ChampionStatistics.Sums.Subscalars.Subscalar()
  }

  implicit object SubscalarsMonoid extends Monoid[ChampionStatistics.Sums.Subscalars] {

    def combine(
      a: ChampionStatistics.Sums.Subscalars,
      b: ChampionStatistics.Sums.Subscalars): ChampionStatistics.Sums.Subscalars = {
      ChampionStatistics.Sums.Subscalars(
        bans = a.bans |+| b.bans,
        allies = a.allies |+| b.allies
      )
    }

    def empty = ChampionStatistics.Sums.Subscalars()
  }

  implicit object SumsMonoid extends Monoid[ChampionStatistics.Sums] {

    def combine(a: ChampionStatistics.Sums, b: ChampionStatistics.Sums): ChampionStatistics.Sums = {
      // TODO(igm): hard part
      ChampionStatistics.Sums(
        scalars = a.scalars |+| b.scalars,
        deltas = a.deltas |+| b.deltas,
        durationDistributions = a.durationDistributions |+| b.durationDistributions,
        subscalars = a.subscalars |+| b.subscalars
      )
    }

    def empty = ChampionStatistics.Sums(
      scalars = Some(ScalarsMonoid.empty),
      deltas = Some(DeltasMonoid.empty),
      durationDistributions = Some(DurationDistributionsMonoid.empty),
      subscalars = Some(SubscalarsMonoid.empty)
    )
  }

}
