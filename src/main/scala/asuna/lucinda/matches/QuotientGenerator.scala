package asuna.lucinda.matches

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import MatchQuotient._
import MatchQuotient.Collections._
import cats.implicits._

object QuotientGenerator {

  // Items that could be core items.
  val coreItems: Set[Int] = Set(
    // TODO(pradyuman)
  )

  def generate(sum: MatchSum): MatchQuotient = {
    // Total number of plays. This is the divisor for the quotient.
    val plays = sum.scalars.map(_.plays).orEmpty
    MatchQuotient(
      scalars = sum.scalars.map(scalars => makeScalars(plays, scalars)),
      deltas = sum.deltas.map(makeDeltas),
      collections = sum.collections.map(colls => makeCollections(plays, colls))
    )
  }

  def makeScalars(plays: Long, sum: MatchSum.Scalars): Scalars = {
    val divScalar = divideScalar(_: Long, plays)
    Scalars(
      wins = divScalar(sum.wins),
      goldEarned = divScalar(sum.goldEarned),
      kills = divScalar(sum.kills),
      deaths = divScalar(sum.deaths),
      assists = divScalar(sum.assists),
      damageDealt = divScalar(sum.damageDealt),
      damageTaken = divScalar(sum.damageTaken),
      minionsKilled = divScalar(sum.minionsKilled),
      teamJungleMinionsKilled = divScalar(sum.teamJungleMinionsKilled),
      enemyJungleMinionsKilled = divScalar(sum.enemyJungleMinionsKilled),
      structureDamage = divScalar(sum.structureDamage),
      killingSpree = divScalar(sum.killingSpree),
      wardsBought = divScalar(sum.wardsBought),
      wardsPlaced = divScalar(sum.wardsPlaced),
      wardsKilled = divScalar(sum.wardsKilled),
      crowdControl = divScalar(sum.crowdControl),
      firstBlood = divScalar(sum.firstBlood),
      firstBloodAssist = divScalar(sum.firstBloodAssist),
      doublekills = divScalar(sum.doublekills),
      triplekills = divScalar(sum.triplekills),
      quadrakills = divScalar(sum.quadrakills),
      pentakills = divScalar(sum.pentakills),
      physicalDamage = divScalar(sum.physicalDamage),
      magicDamage = divScalar(sum.magicDamage),
      trueDamage = divScalar(sum.trueDamage)
    )
  }

  def divideDeltaOpt(
    dd: MatchSum.Deltas.DurationDistribution, deltaOpt: Option[MatchSum.Deltas.Delta]
  ): Option[MatchQuotient.Deltas.Delta] = {
    deltaOpt.map(delta => divideDelta(dd, delta))
  }

  private def divideDelta(
    dd: MatchSum.Deltas.DurationDistribution, delta: MatchSum.Deltas.Delta
  ): MatchQuotient.Deltas.Delta = {
     MatchQuotient.Deltas.Delta(
      zeroToTen = divideScalar(delta.zeroToTen, dd.zeroToTen),
      tenToTwenty = divideScalar(delta.tenToTwenty, dd.tenToTwenty),
      twentyToThirty = divideScalar(delta.twentyToThirty, dd.twentyToThirty),
      thirtyToEnd = divideScalar(delta.thirtyToEnd, dd.thirtyToEnd)
    )
  }

  def makeDeltas(sum: MatchSum.Deltas): Deltas = {
    val divDelta = divideDeltaOpt(sum.durationDistribution.getOrElse(MatchSum.Deltas.DurationDistribution()), _: Option[MatchSum.Deltas.Delta])
    Deltas(
      csDiff = divDelta(sum.csDiff),
      xpDiff = divDelta(sum.xpDiff),
      damageTakenDiff = divDelta(sum.damageTakenDiff),
      xpPerMin = divDelta(sum.xpPerMin),
      goldPerMin = divDelta(sum.goldPerMin),
      towersPerMin = divDelta(sum.towersPerMin),
      wardsPlaced = divDelta(sum.wardsPlaced),
      damageTaken = divDelta(sum.damageTaken)
    )
  }

  private def divideScalar[S](num: S, divisor: Long)(implicit s: scala.math.Numeric[S]): Double = {
    divisor match {
      case plays if plays != 0 => s.toDouble(num) / plays
      case _ => 0
    }
  }

  /**
    * Divides a subscalars by a total.
    */
  def divideSubscalars(total: Long, sum: MatchSum.Collections.Subscalars): Subscalars = {
    Subscalars(
      playRate = divideScalar(sum.plays, total),
      winRate = divideScalar(sum.wins, sum.plays.toLong),
      playCount = sum.plays.toInt
    )
  }

  private def divideSubscalarMap[T](total: Long, map: Map[T, MatchSum.Collections.Subscalars]): Map[T, Subscalars] = {
    map.mapValues(ss => divideSubscalars(total, ss))
  }

  /**
    * Performs a division operation on a list of items containing subscalars.
    */
  def divideSubscalarsList[S, T](
    total: Long,
    sums: Seq[S],
    extractor: S => Option[MatchSum.Collections.Subscalars],
    builder: (S, Option[Subscalars]) => T
  ): Seq[T] = {
    sums.map { sum =>
      builder(sum, divideSubscalars(total, extractor(sum).orEmpty).some)
    }
  }

  /**
    * Constructs our collections objects
    * Static typing is a double edged sword.
    */
  def makeCollections(totalPlays: Long, sum: MatchSum.Collections): Collections = {
    Collections(

      masteries = divideSubscalarsList[MatchSum.Collections.MasterySet, MasterySet](
        totalPlays,
        sum.masteries,
        _.subscalars,
        (s, sc) => MasterySet(masteries = s.masteries, subscalars = sc)
      ),

      runes = divideSubscalarsList[MatchSum.Collections.RuneSet, RuneSet](
        totalPlays,
        sum.runes,
        _.subscalars,
        (s, sc) => RuneSet(runes = s.runes, subscalars = sc)
      ),

      keystones = divideSubscalarsList[MatchSum.Collections.Keystone, Keystone](
        totalPlays,
        sum.keystones,
        _.subscalars,
        (s, sc) => Keystone(keystone = s.keystone, subscalars = sc)
      ),

      summoners = divideSubscalarsList[MatchSum.Collections.SummonerSet, SummonerSet](
        totalPlays,
        sum.summoners,
        _.subscalars,
        (s, sc) => SummonerSet(spell1 = s.spell1, spell2 = s.spell2, subscalars = sc)
      ),

      startingTrinkets = divideSubscalarsList[MatchSum.Collections.Trinket, Trinket](
        totalPlays,
        sum.startingTrinkets,
        _.subscalars,
        (s, sc) => Trinket(trinket = s.trinket, subscalars = sc)
      ),

      endingTrinkets = divideSubscalarsList[MatchSum.Collections.Trinket, Trinket](
        totalPlays,
        sum.endingTrinkets,
        _.subscalars,
        (s, sc) => Trinket(trinket = s.trinket, subscalars = sc)
      ),

      skillOrders = divideSubscalarsList[MatchSum.Collections.SkillOrder, SkillOrder](
        totalPlays,
        mergeSkillOrders(sum.skillOrders),
        _.subscalars,
        (s, sc) => SkillOrder(skillOrder = s.skillOrder, subscalars = sc)
      ),

      durations = divideSubscalarMap(totalPlays, sum.durations),

      bans = divideSubscalarMap(totalPlays, sum.bans),

      allies = divideSubscalarMap(totalPlays, sum.allies),

      enemies = divideSubscalarMap(totalPlays, sum.enemies),

      starterItems = divideSubscalarsList[MatchSum.Collections.ItemList, ItemList](
        totalPlays,
        sum.starterItems,
        _.subscalars,
        (s, sc) => ItemList(items = s.items, subscalars = sc)
      ),

      coreBuilds = divideSubscalarsList[MatchSum.Collections.ItemList, ItemList](
        totalPlays,
        sum.coreBuilds,
        _.subscalars,
        (s, sc) => ItemList(items = s.items, subscalars = sc)
      )

    )
  }

  /**
    * Merges skill orders lesser than 18 in length.
    */
  def mergeSkillOrders(skillOrders: Seq[MatchSum.Collections.SkillOrder]): Seq[MatchSum.Collections.SkillOrder] = {
    // TODO(igm): this is pretty inefficient. we can use a trie for slightly faster calculations.
    // Verify if this pretty code is worth it.

    // build a map of the skill orders
    val soMap = skillOrders
      .groupBy(_.skillOrder)
      .mapValues(_.headOption.getOrElse(MatchSum.Collections.Subscalars()))

    // find all matching prefix
    skillOrders.filter(_.skillOrder.size == 18).map { order =>
      val so = order.skillOrder
      val subscalars = skillOrders
        // check if skill orders are the same
        .filter(k => so.take(k.skillOrder.size) == k.skillOrder)
        // combine
        .toList.map(_.subscalars).combineAll
      order.copy(subscalars = subscalars)
    }
  }

}
