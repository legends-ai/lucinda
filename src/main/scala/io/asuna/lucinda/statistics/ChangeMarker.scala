package io.asuna.lucinda.statistics

import scala.collection.immutable.Map
import cats.implicits._
import cats.Semigroup
import io.asuna.proto.lucinda.LucindaData.Statistic
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics

object ChangeMarker {

  implicit object MarkStatisticSemigroup extends Semigroup[Statistic] {
    def combine(stat: Statistic, prev: Statistic): Statistic = {
      stat.copy(change = prev.rank - stat.rank)
    }
  }

  implicit class CombineStatisticMaps(stat: Map[Int, Statistic]) {
    def |++|(prev: Map[Int, Statistic]) = {
      stat.transform { (key, value) =>
        prev.get(key) match {
          case Some(v) => value |+| v
          case None => value
        }
      }
    }
  }

  implicit object MarkDeltaSemigroup extends Semigroup[ChampionStatistics.Results.Deltas.Delta] {
    def combine(
      delta: ChampionStatistics.Results.Deltas.Delta, prev: ChampionStatistics.Results.Deltas.Delta
    ) = {
      ChampionStatistics.Results.Deltas.Delta(
        zeroToTen = delta.zeroToTen |++| prev.zeroToTen,
        tenToTwenty = delta.tenToTwenty |++| prev.tenToTwenty,
        twentyToThirty = delta.twentyToThirty |++| prev.twentyToThirty,
        thirtyToEnd = delta.thirtyToEnd |++| prev.thirtyToEnd
      )
    }
  }

  implicit object MarkScalarsSemigroup extends Semigroup[ChampionStatistics.Results.Scalars] {
    def combine(
      scalars: ChampionStatistics.Results.Scalars, prev: ChampionStatistics.Results.Scalars
    ) = {
      ChampionStatistics.Results.Scalars(
        wins = scalars.wins |++| prev.wins,
        goldEarned = scalars.goldEarned |++| prev.goldEarned,
        kills = scalars.kills |++| prev.kills,
        deaths = scalars.deaths |++| prev.deaths,
        assists = scalars.assists |++| prev.assists,
        damageDealt = scalars.damageDealt |++| prev.damageDealt,
        damageTaken = scalars.damageTaken |++| prev.damageTaken,
        minionsKilled = scalars.minionsKilled |++| prev.minionsKilled,
        teamJungleMinionsKilled = scalars.teamJungleMinionsKilled |++| prev.teamJungleMinionsKilled,
        enemyJungleMinionsKilled = scalars.enemyJungleMinionsKilled |++| prev.enemyJungleMinionsKilled,
        structureDamage = scalars.structureDamage |++| prev.structureDamage,
        killingSpree = scalars.killingSpree |++| prev.killingSpree,
        wardsBought = scalars.wardsBought |++| prev.wardsBought,
        wardsPlaced = scalars.wardsPlaced |++| prev.wardsPlaced,
        wardsKilled = scalars.wardsKilled |++| prev.wardsKilled,
        crowdControl = scalars.crowdControl |++| prev.crowdControl,
        firstBlood = scalars.firstBlood |++| prev.firstBlood,
        firstBloodAssist = scalars.firstBloodAssist |++| prev.firstBloodAssist,
        doublekills = scalars.doublekills |++| prev.doublekills,
        triplekills = scalars.triplekills |++| prev.triplekills,
        quadrakills = scalars.quadrakills |++| prev.quadrakills,
        pentakills = scalars.pentakills |++| prev.pentakills,
        physicalDamage = scalars.physicalDamage |++| prev.physicalDamage,
        magicDamage = scalars.magicDamage |++| prev.magicDamage,
        trueDamage = scalars.trueDamage |++| prev.trueDamage
      )
    }
  }

  implicit object MarkDeltasSemigroup extends Semigroup[ChampionStatistics.Results.Deltas] {
    def combine(
      deltas: ChampionStatistics.Results.Deltas, prev: ChampionStatistics.Results.Deltas
    ) = {
      ChampionStatistics.Results.Deltas(
        csDiff = deltas.csDiff |+| prev.csDiff,
        xpDiff = deltas.xpDiff |+| prev.xpDiff,
        damageTakenDiff = deltas.damageTakenDiff |+| prev.damageTakenDiff,
        xpPerMin = deltas.xpPerMin |+| prev.xpPerMin,
        goldPerMin = deltas.goldPerMin |+| prev.goldPerMin,
        towersPerMin = deltas.towersPerMin |+| prev.towersPerMin,
        wardsPlaced = deltas.wardsPlaced |+| prev.wardsPlaced,
        damageTaken = deltas.damageTaken |+| prev.damageTaken
      )
    }
  }

  implicit object MarkDerivativesSemigroup extends Semigroup[ChampionStatistics.Results.Derivatives] {
    def combine(
      derivatives: ChampionStatistics.Results.Derivatives, prev: ChampionStatistics.Results.Derivatives
    ) = {
      ChampionStatistics.Results.Derivatives(
        picks = derivatives.picks |+| prev.picks,
        bans = derivatives.bans |+| prev.bans
      )
    }
  }

  implicit object MarkResultsSemigroup extends Semigroup[ChampionStatistics.Results] {
    def combine(
      stats: ChampionStatistics.Results, prev: ChampionStatistics.Results
    ): ChampionStatistics.Results = {
      stats.copy(
        scalars = stats.scalars |+| prev.scalars,
        deltas = stats.deltas |+| prev.deltas,
        derivatives = stats.derivatives |+| prev.derivatives
      )
    }
  }

  def mark(stats: ChampionStatistics, prev: ChampionStatistics) =
    stats.copy(results = stats.results |+| prev.results)

}
