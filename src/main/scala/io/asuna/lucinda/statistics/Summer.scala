package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.Sums
import io.asuna.proto.match_sum.MatchSum

object Summer {

  def makeSums(sums: Map[Int, MatchSum]): Sums = {
    sums.foldLeft(Sums()) { case (agg, (champion, sum)) =>
      Sums(
        scalars = Some(
          agg.scalars.getOrElse(Sums.Scalars())
            .append(champion, sum.scalars.getOrElse(MatchSum.Scalars()))
        ),
        deltas = Some(
          agg.deltas.getOrElse(Sums.Deltas())
            .append(champion, sum.deltas.getOrElse(MatchSum.Deltas()))
        )
      )
    }
  }

  private implicit class ScalarsAggregator(agg: Sums.Scalars) {

    def append(champion: Int, scalars: MatchSum.Scalars): Sums.Scalars = {
      Sums.Scalars(
        plays = agg.plays + (champion -> scalars.plays),
        wins = agg.wins + (champion -> scalars.wins),
        goldEarned = agg.goldEarned + (champion -> scalars.goldEarned),
        kills = agg.kills + (champion -> scalars.kills),
        deaths = agg.deaths + (champion -> scalars.deaths),
        assists = agg.assists + (champion -> scalars.assists),
        damageDealt = agg.damageDealt + (champion -> scalars.damageDealt),
        damageTaken = agg.damageTaken + (champion -> scalars.damageTaken),
        minionsKilled = agg.minionsKilled + (champion -> scalars.minionsKilled),
        teamJungleMinionsKilled = agg.teamJungleMinionsKilled + (champion -> scalars.teamJungleMinionsKilled),
        enemyJungleMinionsKilled = agg.enemyJungleMinionsKilled + (champion -> scalars.enemyJungleMinionsKilled),
        structureDamage = agg.structureDamage + (champion -> scalars.structureDamage),
        killingSpree = agg.killingSpree + (champion -> scalars.killingSpree),
        wardsBought = agg.wardsBought + (champion -> scalars.wardsBought),
        wardsPlaced = agg.wardsPlaced + (champion -> scalars.wardsPlaced),
        wardsKilled = agg.wardsKilled + (champion -> scalars.wardsKilled),
        crowdControl = agg.crowdControl + (champion -> scalars.crowdControl),
        firstBlood = agg.firstBlood + (champion -> scalars.firstBlood),
        firstBloodAssist = agg.firstBloodAssist + (champion -> scalars.firstBloodAssist),
        doublekills = agg.doublekills + (champion -> scalars.doublekills),
        triplekills = agg.triplekills + (champion -> scalars.triplekills),
        quadrakills = agg.quadrakills + (champion -> scalars.quadrakills),
        pentakills = agg.pentakills + (champion -> scalars.pentakills),
        physicalDamage = agg.physicalDamage + (champion -> scalars.physicalDamage),
        magicDamage = agg.magicDamage + (champion -> scalars.magicDamage),
        trueDamage = agg.trueDamage + (champion -> scalars.trueDamage)
      )
    }

  }

  private implicit class DeltasAggregator(agg: Sums.Deltas) {

    def append(champion: Int, deltas: MatchSum.Deltas): Sums.Deltas = {
      Sums.Deltas(
        csDiff = agg.csDiff.append(champion, deltas.csDiff),
        xpDiff = agg.xpDiff.append(champion, deltas.xpDiff),
        damageTakenDiff = agg.damageTakenDiff.append(champion, deltas.damageTakenDiff),
        xpPerMin = agg.xpPerMin.append(champion, deltas.xpPerMin),
        goldPerMin = agg.goldPerMin.append(champion, deltas.goldPerMin),
        towersPerMin = agg.towersPerMin.append(champion, deltas.towersPerMin),
        wardsPlaced = agg.wardsPlaced.append(champion, deltas.wardsPlaced),
        damageTaken = agg.damageTaken.append(champion, deltas.damageTaken)
      )
    }

  }

  private implicit class DeltaOptionAggregator(agg: Option[Sums.Deltas.Delta]) {
    def append(champion: Int, delta: Option[MatchSum.Deltas.Delta]): Option[Sums.Deltas.Delta] = {
      val delt = delta.getOrElse(MatchSum.Deltas.Delta())
      agg match {
        case Some(d) => Some(d.append(champion, delt))
        case None => Some(Sums.Deltas.Delta().append(champion, delt))
      }
    }
  }

  private implicit class DeltaAggregator(agg: Sums.Deltas.Delta) {

    def append(champion: Int, delta: MatchSum.Deltas.Delta): Sums.Deltas.Delta = {
      Sums.Deltas.Delta(
        zeroToTen = agg.zeroToTen + (champion -> delta.zeroToTen),
        tenToTwenty = agg.tenToTwenty + (champion -> delta.tenToTwenty),
        twentyToThirty = agg.twentyToThirty + (champion -> delta.twentyToThirty),
        thirtyToEnd = agg.thirtyToEnd + (champion -> delta.thirtyToEnd)
      )
    }

  }

}
