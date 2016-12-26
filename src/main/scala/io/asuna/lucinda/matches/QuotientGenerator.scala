package io.asuna.lucinda.matches

import cats.implicits._
import io.asuna.proto.match_quotient.MatchQuotient
import io.asuna.proto.match_sum.MatchSum
import io.asuna.asunasan.legends.MatchSumHelpers._

object QuotientGenerator {

  def generate(sum: MatchSum): MatchQuotient = {
    val justPlays = sum.scalars.map(_.plays)

    val divScalar = divideScalar(_: Long, justPlays)
    val divDelta = divideDelta(sum.durationDistribution, _: Option[MatchSum.Deltas.Delta])

    val scalars = sum.scalars.getOrElse(MatchSum.Scalars())
    val deltas = sum.deltas.getOrElse(MatchSum.Deltas())

    MatchQuotient(
      scalars = Some(MatchQuotient.Scalars(
        wins = divScalar(scalars.wins),
        goldEarned = divScalar(scalars.goldEarned),
        kills = divScalar(scalars.kills),
        deaths = divScalar(scalars.deaths),
        assists = divScalar(scalars.assists),
        damageDealt = divScalar(scalars.damageDealt),
        damageTaken = divScalar(scalars.damageTaken),
        minionsKilled = divScalar(scalars.minionsKilled),
        teamJungleMinionsKilled = divScalar(scalars.teamJungleMinionsKilled),
        enemyJungleMinionsKilled = divScalar(scalars.enemyJungleMinionsKilled),
        structureDamage = divScalar(scalars.structureDamage),
        killingSpree = divScalar(scalars.killingSpree),
        wardsBought = divScalar(scalars.wardsBought),
        wardsPlaced = divScalar(scalars.wardsPlaced),
        wardsKilled = divScalar(scalars.wardsKilled),
        crowdControl = divScalar(scalars.crowdControl),
        firstBlood = divScalar(scalars.firstBlood),
        firstBloodAssist = divScalar(scalars.firstBloodAssist),
        doublekills = divScalar(scalars.doublekills),
        triplekills = divScalar(scalars.triplekills),
        quadrakills = divScalar(scalars.quadrakills),
        pentakills = divScalar(scalars.pentakills),
        physicalDamage = divScalar(scalars.physicalDamage),
        magicDamage = divScalar(scalars.magicDamage),
        trueDamage = divScalar(scalars.trueDamage)
      )),
      deltas = Some(MatchQuotient.Deltas(
        csDiff = divDelta(deltas.csDiff),
        xpDiff = divDelta(deltas.xpDiff),
        damageTakenDiff = divDelta(deltas.damageTakenDiff),
        xpPerMin = divDelta(deltas.xpPerMin),
        goldPerMin = divDelta(deltas.goldPerMin),
        towersPerMin = divDelta(deltas.towersPerMin),
        wardsPlaced = divDelta(deltas.wardsPlaced),
        damageTaken = divDelta(deltas.damageTaken)
      )),
      masteries = divideSubscalarMap(justPlays, sum.masteries),
      runes = divideSubscalarMap(justPlays, sum.runes),
      keystones = divideSubscalarMap(justPlays, sum.keystones),
      summoners = divideSubscalarMap(justPlays, sum.summoners),
      trinkets = divideSubscalarMap(justPlays, sum.trinkets),
      skillOrders = divideSubscalarMap(justPlays, mergeSkillOrders(sum.skillOrders)),
      durations = divideSubscalarMap(justPlays, sum.durations),
      bans = divideSubscalarMap(justPlays, sum.bans),
      allies = divideSubscalarMap(justPlays, sum.allies),
      enemies = divideSubscalarMap(justPlays, sum.enemies),
      starterItems = divideSubscalarMap(justPlays, sum.starterItems),
      buildPath = divideSubscalarMap(justPlays, sum.buildPath),
      coreBuildList = divideSubscalarMap(justPlays, sum.buildPath)
    )
  }

  private def divideScalar[S](num: S, divisor: Option[Long])(implicit s: scala.math.Numeric[S]): Double = {
    divisor match {
      case Some(plays) if plays != 0 => s.toDouble(num) / plays
      case _ => 0
    }
  }

  private def divideDelta(
    dd: Option[MatchSum.DurationDistribution], deltaOpt: Option[MatchSum.Deltas.Delta]
  ): Option[MatchQuotient.Deltas.Delta] = {
    for {
      delta <- deltaOpt
    } yield MatchQuotient.Deltas.Delta(
      zeroToTen = divideScalar(delta.zeroToTen, dd.map(_.zeroToTen)),
      tenToTwenty = divideScalar(delta.tenToTwenty, dd.map(_.tenToTwenty)),
      twentyToThirty = divideScalar(delta.twentyToThirty, dd.map(_.twentyToThirty)),
      thirtyToEnd = divideScalar(delta.thirtyToEnd, dd.map(_.thirtyToEnd))
    )
  }

  private def divideSubscalarMap[T](total: Option[Long], map: Map[T, MatchSum.Subscalars]): Map[T, MatchQuotient.Subscalars] = {
    map.mapValues { subscalars =>
      MatchQuotient.Subscalars(
        plays = divideScalar(subscalars.plays, total),
        wins = divideScalar(subscalars.wins, total),
        playCount = subscalars.plays
      )
    }
  }

  /**
    * Merges skill orders lesser than 18 in length.
    */
  def mergeSkillOrders(skillOrders: Map[String, MatchSum.Subscalars]): Map[String, MatchSum.Subscalars] = {
    // TODO(igm): this is pretty inefficient. we can use a trie for slightly faster calculations.
    // Verify if this is worth it.
    skillOrders.filterKeys(_.length() == 18).transform { (skillOrder, _) =>
      skillOrders.filterKeys(skillOrder startsWith _).values.toList.combineAll
    }
  }

}
