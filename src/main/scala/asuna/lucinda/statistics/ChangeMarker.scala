package asuna.lucinda.statistics

import scala.collection.immutable.Map

import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistic }
import cats.implicits._
import cats.Semigroup

object ChangeMarker {

  implicit object MarkStatisticSemigroup extends Semigroup[Statistic] {
    def combine(stat: Statistic, prev: Statistic): Statistic = {
      stat.copy(
        changeInRank = prev.rank - stat.rank,
        changeInValue = prev.value - stat.value
      )
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

  implicit object MarkDeltaSemigroup extends Semigroup[AllChampionStatistics.Results.Deltas.Delta] {
    def combine(
      delta: AllChampionStatistics.Results.Deltas.Delta, prev: AllChampionStatistics.Results.Deltas.Delta
    ) = {
      AllChampionStatistics.Results.Deltas.Delta(
        zeroToTen = delta.zeroToTen |++| prev.zeroToTen,
        tenToTwenty = delta.tenToTwenty |++| prev.tenToTwenty,
        twentyToThirty = delta.twentyToThirty |++| prev.twentyToThirty,
        thirtyToEnd = delta.thirtyToEnd |++| prev.thirtyToEnd
      )
    }
  }

  implicit object MarkScalarsSemigroup extends Semigroup[AllChampionStatistics.Results.Scalars] {
    def combine(
      scalars: AllChampionStatistics.Results.Scalars, prev: AllChampionStatistics.Results.Scalars
    ) = {
      AllChampionStatistics.Results.Scalars(
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

  implicit object MarkDeltasSemigroup extends Semigroup[AllChampionStatistics.Results.Deltas] {
    def combine(
      deltas: AllChampionStatistics.Results.Deltas, prev: AllChampionStatistics.Results.Deltas
    ) = {
      AllChampionStatistics.Results.Deltas(
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

  implicit object MarkDerivativesSemigroup extends Semigroup[AllChampionStatistics.Results.Derivatives] {
    def combine(
      derivatives: AllChampionStatistics.Results.Derivatives, prev: AllChampionStatistics.Results.Derivatives
    ) = {
      AllChampionStatistics.Results.Derivatives(
        picks = derivatives.picks |+| prev.picks,
        bans = derivatives.bans |+| prev.bans
      )
    }
  }

  implicit object MarkResultsSemigroup extends Semigroup[AllChampionStatistics.Results] {
    def combine(
      stats: AllChampionStatistics.Results, prev: AllChampionStatistics.Results
    ): AllChampionStatistics.Results = {
      stats.copy(
        scalars = stats.scalars |+| prev.scalars,
        deltas = stats.deltas |+| prev.deltas,
        derivatives = stats.derivatives |+| prev.derivatives
      )
    }
  }

  def mark(stats: AllChampionStatistics, prev: AllChampionStatistics) =
    stats.copy(
      results = stats.results |+| prev.results
    )

}
