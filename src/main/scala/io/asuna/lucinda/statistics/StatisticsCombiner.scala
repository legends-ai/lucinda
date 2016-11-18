package io.asuna.lucinda.statistics

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

  implicit object SumsMonoid extends Monoid[ChampionStatistics.Sums] {
    def append(a: ChampionStatistics.Sums, b: => ChampionStatistics.Sums): ChampionStatistics.Sums = {
      // TODO(igm): hard part
      ChampionStatistics.Sums(
        scalars = a.scalars |+| b.scalars
          // TODO(pradyuman): implement
      )
    }

    def zero = ChampionStatistics.Sums()
  }

}

object StatisticsCombiner extends StatisticsMonoids {

  def combineMulti(aList: Iterable[ChampionStatistics]): ChampionStatistics = {
    if (aList.size == 0) {
      return ChampionStatistics()
    }
    combine(aList.head, combineMulti(aList.tail))
  }

  def combine(a: ChampionStatistics, b: ChampionStatistics): ChampionStatistics = {
    val aRole = a.statistics.groupBy(_.role).mapValues(_.head)
    val bRole = b.statistics.groupBy(_.role).mapValues(_.head)
    val combinedList = for {
      role <- aRole.keys ++ bRole.keys
    } yield combineStats(
      role, aRole.getOrElse(role, ChampionStatistics.Statistics()),
      bRole.getOrElse(role, ChampionStatistics.Statistics())
    )
    ChampionStatistics(statistics = combinedList.toSeq)
  }

  def combineStats(role: Role, a: ChampionStatistics.Statistics, b: ChampionStatistics.Statistics): ChampionStatistics.Statistics = {
    val sums = a.sums |+| b.sums
    val quotients = sums.map(QuotientsGenerator.generateQuotients)
    val results = quotients.map(ResultsGenerator.generateResults)
    ChampionStatistics.Statistics(
      role = role,
      results = results,
      quotients = quotients,
      sums = sums
    )
  }

}
