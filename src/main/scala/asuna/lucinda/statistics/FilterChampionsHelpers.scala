package asuna.lucinda.statistics

import asuna.proto.league.lucinda.AllChampionStatistics.Results

object FilterChampionsHelpers {

  implicit class ScalarsFilterChampions(scalars: Results.Scalars) {

    def filterChampions(champs: Set[Int]): Results.Scalars = {
      scalars.copy(
        wins = scalars.wins.filterKeys(champs),
        goldEarned = scalars.goldEarned.filterKeys(champs),
        kills = scalars.kills.filterKeys(champs),
        deaths = scalars.deaths.filterKeys(champs),
        assists = scalars.assists.filterKeys(champs),
        damageDealt = scalars.damageDealt.filterKeys(champs),
        damageTaken = scalars.damageTaken.filterKeys(champs),
        minionsKilled = scalars.minionsKilled.filterKeys(champs),
        teamJungleMinionsKilled = scalars.teamJungleMinionsKilled.filterKeys(champs),
        enemyJungleMinionsKilled = scalars.enemyJungleMinionsKilled.filterKeys(champs),
        killingSpree = scalars.killingSpree.filterKeys(champs),
        wardsBought = scalars.wardsBought.filterKeys(champs),
        wardsPlaced = scalars.wardsPlaced.filterKeys(champs),
        wardsKilled = scalars.wardsKilled.filterKeys(champs),
        crowdControl = scalars.crowdControl.filterKeys(champs),
        firstBlood = scalars.firstBlood.filterKeys(champs),
        firstBloodAssist = scalars.firstBloodAssist.filterKeys(champs),
        doublekills = scalars.doublekills.filterKeys(champs),
        triplekills = scalars.triplekills.filterKeys(champs),
        quadrakills = scalars.quadrakills.filterKeys(champs),
        pentakills = scalars.pentakills.filterKeys(champs),
        physicalDamage = scalars.physicalDamage.filterKeys(champs),
        magicDamage = scalars.magicDamage.filterKeys(champs),
        trueDamage = scalars.trueDamage.filterKeys(champs)
      )
    }

  }

  implicit class DeltaFilterChampions(delta: Results.Deltas.Delta) {

    def filterChampions(champs: Set[Int]): Results.Deltas.Delta = {
      delta.copy(
        zeroToTen = delta.zeroToTen.filterKeys(champs),
        tenToTwenty = delta.tenToTwenty.filterKeys(champs),
        twentyToThirty = delta.twentyToThirty.filterKeys(champs),
        thirtyToEnd = delta.thirtyToEnd.filterKeys(champs)
      )
    }

  }

  implicit class DeltasFilterChampions(deltas: Results.Deltas) {

    def filterChampions(champs: Set[Int]): Results.Deltas = {
      deltas.copy(
        csDiff = deltas.csDiff.map(_.filterChampions(champs)),
        xpDiff = deltas.xpDiff.map(_.filterChampions(champs)),
        damageTakenDiff = deltas.damageTakenDiff.map(_.filterChampions(champs)),
        xpPerMin = deltas.xpPerMin.map(_.filterChampions(champs)),
        goldPerMin = deltas.goldPerMin.map(_.filterChampions(champs)),
        towersPerMin = deltas.towersPerMin.map(_.filterChampions(champs)),
        wardsPlaced = deltas.wardsPlaced.map(_.filterChampions(champs)),
        damageTaken = deltas.damageTaken.map(_.filterChampions(champs))
      )
    }

  }

  implicit class DerivativesFilterChampions(derivatives: Results.Derivatives) {

    def filterChampions(champs: Set[Int]): Results.Derivatives = {
      derivatives.copy(
        picks = derivatives.picks.filterKeys(champs),
        bans = derivatives.bans.filterKeys(champs)
      )
    }

  }

  implicit class ResultsFilterChampions(results: Results) {

    def filterChampions(champs: Set[Int]): Results = {
      results.copy(
        scalars = results.scalars.map(_.filterChampions(champs)),
        deltas = results.deltas.map(_.filterChampions(champs)),
        derivatives = results.derivatives.map(_.filterChampions(champs))
      )
    }

  }

}
