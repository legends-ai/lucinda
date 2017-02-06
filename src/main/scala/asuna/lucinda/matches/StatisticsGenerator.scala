package asuna.lucinda.matches

import scala.util.{ Success, Try }

import asuna.common.legends.MatchSumHelpers._
import asuna.lucinda.statistics.StatisticsCombiner._
import asuna.proto.league.{ Ability, IntRange, MatchSum, Region, Role }
import asuna.proto.league.lucinda.{ AllChampionStatistics, MatchQuotient, Statistic }
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.lucinda.Statistics.Graphs.GoldPerTime
import cats.implicits._

object StatisticsGenerator {

  def makeStatistics(
    champion: Int,
    minPlayRate: Double,
    patchStats: Map[String, AllChampionStatistics],
    byRole: Map[Role, MatchSum],
    byPatch: Map[String, MatchSum]
  ): Statistics = {
    // First, we will combine all statistics objects from all patches in the range.
    // This uses the StatisticsMonoid.
    val allPatches = patchStats.values.toList
    val allStats = allPatches.headOption match {
      case Some(patch) => allPatches.combineAll.copy(role = patch.role)
      case None => allPatches.combineAll
    }

    // This is the quotient of the champion for the entire search space.
    val quot = QuotientGenerator.generate(byPatch.values.toList.combineAll)

    Statistics(
      roles = makeRoleStats(allStats, byRole).some,
      scalars = makeScalars(champion, allStats).some,
      deltas = makeDeltas(champion, allStats).some,
      graphs = makeGraphs(allStats, patchStats, quot, champion).some,
      collections = makeCollections(quot, minPlayRate).some
    )
  }

  /**
    * Prepares the StatisticsRoles object.
    */
  private[this] def makeRoleStats(allStats: AllChampionStatistics, roleSums: Map[Role, MatchSum]): Statistics.Roles = {
    // We get the total champions in role based on number of win rates in map.
    val totalChampionsInRole = allStats.results.flatMap(_.scalars)
      .map(_.wins.keys.filter(_ > 0).size).getOrElse(0)

    // Number of games played by the champion for each role.
    val gamesByRole = roleSums.mapValues(_.scalars.map(_.plays).getOrElse(0L)).toMap

    // Number of total games played by the champion.
    val totalGames = gamesByRole.values.sum

    // Stats by role.
    val roleStats = for {
      (role, numMatches) <- gamesByRole
    } yield Statistics.Roles.RoleStats(
      role = role,
      pickRate = if (totalGames == 0) 0 else (numMatches.toDouble / totalGames),
      numMatches = numMatches.toInt
    )

    Statistics.Roles(
      role = allStats.role,
      totalChampionsInRole = totalChampionsInRole,
      roleStats = roleStats.toSeq
    )
  }

  private def getStat[T](obj: Option[T], champion: Int, accessor: T => Map[Int, Statistic]): Option[Statistic] = {
    obj.flatMap(accessor(_).get(champion)).getOrElse(Statistic()).some
  }

  private def getDelta(deltas: Option[AllChampionStatistics.Results.Deltas], champion: Int, accessor: AllChampionStatistics.Results.Deltas => Option[AllChampionStatistics.Results.Deltas.Delta]): Option[Statistics.Deltas.Delta] = {
    val delta = deltas.flatMap(accessor)
    val get = getStat(delta, champion, _: AllChampionStatistics.Results.Deltas.Delta => Map[Int, Statistic])
    if (!delta.isDefined) {
      return None
    }
    Some(Statistics.Deltas.Delta(
      zeroToTen = get(_.zeroToTen),
      tenToTwenty = get(_.tenToTwenty),
      twentyToThirty = get(_.twentyToThirty),
      thirtyToEnd = get(_.thirtyToEnd)
    ))
  }

  def makeScalars(champion: Int, roleStats: AllChampionStatistics): Statistics.Scalars = {
    val getScalar = getStat(
      roleStats.results.flatMap(_.scalars),
      champion, _: AllChampionStatistics.Results.Scalars => Map[Int, Statistic]
    )
    val derivatives = roleStats.results.flatMap(_.derivatives)

    // We calculate the pick rate stat out here, as we need it to find the gamesPlayed stat.
    val pickRateStat = getStat[AllChampionStatistics.Results.Derivatives](derivatives, champion, _.picks)

    // Number of games played by this champion in this role.
    val gamesPlayed = roleStats.sums.flatMap(_.scalars).flatMap(_.plays.get(champion)).getOrElse(0L)

    // We can derive the gamesPlayed stat from the pickRate stat as they are virtually identical.
    val gamesPlayedStat = pickRateStat.map { stat =>
      stat
      // First, let's set the correct value for games played.
        .update(_.value := gamesPlayed.toDouble)
      // The average can be derived by finding the multiplier of the value.
        .update(_.average := ((stat.average / stat.value) * gamesPlayed.toDouble).floor)

      // All other attributes of stat (rank, change, champ) are the same, so we are done.
    }

    Statistics.Scalars(
      winRate = getScalar(_.wins),
      pickRate = pickRateStat,
      banRate = getStat[AllChampionStatistics.Results.Derivatives](derivatives, champion, _.bans),
      gamesPlayed = gamesPlayedStat,
      goldEarned = getScalar(_.goldEarned),
      kills = getScalar(_.kills),
      deaths = getScalar(_.deaths),
      assists = getScalar(_.assists),
      damageDealt = getScalar(_.damageDealt),
      damageTaken = getScalar(_.damageTaken),
      minionsKilled = getScalar(_.minionsKilled),
      teamJungleMinionsKilled = getScalar(_.teamJungleMinionsKilled),
      enemyJungleMinionsKilled = getScalar(_.enemyJungleMinionsKilled),
      structureDamage = getScalar(_.structureDamage),
      killingSpree = getScalar(_.killingSpree),
      wardsBought = getScalar(_.wardsBought),
      wardsPlaced = getScalar(_.wardsPlaced),
      wardsKilled = getScalar(_.wardsKilled),
      crowdControl = getScalar(_.crowdControl),
      firstBlood = getScalar(_.firstBlood),
      firstBloodAssist = getScalar(_.firstBloodAssist),
      doublekills = getScalar(_.doublekills),
      triplekills = getScalar(_.triplekills),
      quadrakills = getScalar(_.quadrakills),
      pentakills = getScalar(_.pentakills)
    )
  }

  def makeDeltas(champion: Int, roleStats: AllChampionStatistics): Statistics.Deltas = {
    val getDeltas = getDelta(
      roleStats.results.flatMap(_.deltas),
      champion, _: AllChampionStatistics.Results.Deltas => Option[AllChampionStatistics.Results.Deltas.Delta]
    )
    Statistics.Deltas(
      csDiff = getDeltas(_.csDiff),
      xpDiff = getDeltas(_.xpDiff),
      damageTakenDiff = getDeltas(_.damageTakenDiff),
      xpPerMin = getDeltas(_.xpPerMin),
      goldPerMin = getDeltas(_.goldPerMin),
      towersPerMin = getDeltas(_.towersPerMin),
      wardsPlaced = getDeltas(_.wardsPlaced),
      damageTaken = getDeltas(_.damageTaken)
    )
  }

  /**
    * Makes our graphs.
    *
    * @param patchStats: Map of patch to results for the role.
    */
  def makeGraphs(
    roleStats: AllChampionStatistics,
    patchStats: Map[String, AllChampionStatistics],
    quot: MatchQuotient,
    id: Int
  ): Statistics.Graphs = {
    val results = roleStats.results
    Statistics.Graphs(
      // Win/pick/ban distribution across all champions.
      distribution = Some(Statistics.Graphs.Distribution(
        winRate = results.flatMap(_.scalars).map(_.wins.mapValues(_.value)).orEmpty,
        pickRate = results.flatMap(_.derivatives).map(_.picks.mapValues(_.value)).orEmpty,
        banRate = results.flatMap(_.derivatives).map(_.bans.mapValues(_.value)).orEmpty
      )),

      // Per-patch statistics.
      // TODO(igm): investigate why this code is so freaking ugly
      byPatch = patchStats.mapValues(_.results).map { case (patch, patchResults) =>
        Statistics.Graphs.ByPatch(
          patch = patch,
          winRate = patchResults.flatMap(_.scalars).flatMap(_.wins.mapValues(_.value).get(id)).orEmpty,
          pickRate = patchResults.flatMap(_.derivatives).flatMap(_.picks.mapValues(_.value).get(id)).orEmpty,
          banRate = patchResults.flatMap(_.derivatives).flatMap(_.bans.mapValues(_.value).get(id)).orEmpty
        )
      }.toSeq,

      byGameLength = quot.durations.map { case (duration, stats) =>
        Statistics.Graphs.ByGameLength(
          gameLength = Some(IntRange(min = duration, max = duration)),
          winRate = stats.wins
        )
      }.toSeq,

      physicalDamage = results.flatMap(_.scalars).flatMap(_.physicalDamage.get(id)).map(_.value).orEmpty,
      magicDamage = results.flatMap(_.scalars).flatMap(_.magicDamage.get(id)).map(_.value).orEmpty,
      trueDamage = results.flatMap(_.scalars).flatMap(_.trueDamage.get(id)).map(_.value).orEmpty,

      goldOverTime = {
        val gpm = quot.deltas.flatMap(_.goldPerMin)
        List(
          GoldPerTime(
            gold = gpm.map(_.zeroToTen).orEmpty,
            time = Some(IntRange(min = 0, max = 10))
          ),
          GoldPerTime(
            gold = gpm.map(_.tenToTwenty).orEmpty,
            time = Some(IntRange(min = 10, max = 20))
          ),
          GoldPerTime(
            gold = gpm.map(_.twentyToThirty).orEmpty,
            time = Some(IntRange(min = 20, max = 30))
          ),
          GoldPerTime(
            gold = gpm.map(_.thirtyToEnd).orEmpty,
            time = Some(IntRange(min = 30, max = 40))
          )
        )
      }
    )
  }

  implicit class SafeMap[T](coll: Traversable[T]) {
    def safelyMap[U](f: T => U): Traversable[U] = {
      coll.map(el => Try { f(el) }) collect { case Success(x) => x }
    }
  }

  /**
    * Makes our collections.
    */
  def makeCollections(quot: MatchQuotient, minPlayRate: Double): Statistics.Collections = {
    // TODO(igm): figure out how we can do more code reuse here.
    Statistics.Collections(
      runes = quot.runes
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (runeSet, subscalars) =>
          Statistics.Collections.RuneSet(
            runes = deserializeBonusSet(runeSet),
            subscalars = Some(subscalars)
          )
      }.toSeq,

      masteries = quot.masteries
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (masterySet, subscalars) =>
          Statistics.Collections.MasterySet(
            masteries = deserializeBonusSet(masterySet),
            subscalars = Some(subscalars)
          )
      }.toSeq,

      summonerSpells = quot.summoners
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (spells, subscalars) =>
          val (spell1, spell2) = deserializeSummoners(spells)
          Statistics.Collections.SummonerSet(
            spell1 = spell1,
            spell2 = spell2,
            subscalars = Some(subscalars)
          )
      }.toSeq,

      skillOrders = quot.skillOrders
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (skillOrder, subscalars) =>
          Statistics.Collections.SkillOrder(
            skillOrder = deserializeSkillOrder(skillOrder),
            subscalars = Some(subscalars)
          )
      }.toSeq,

      starterItems = quot.starterItems
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (build, subscalars) =>
          Statistics.Collections.Build(
            build = deserializeBuild(build),
            subscalars = Some(subscalars)
          )
      }.toSeq,

      buildPath = quot.buildPath
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .safelyMap { case (build, subscalars) =>
          Statistics.Collections.Build(
            build = deserializeBuild(build),
            subscalars = Some(subscalars)
          )
      }.toSeq

    )
  }

  def deserializeBonusSet(serialized: String): Map[Int, Int] = {
    serialized.split("\\|").map { part =>
      var element = part.split(":").map(_.toInt)
      (element(0), element(1))
    }.toMap
  }

  def deserializeSummoners(serialized: String): (Int, Int) = {
    val Array(a, b) = serialized.split("\\|")
    (a.toInt, b.toInt)
  }

  def deserializeSkillOrder(serialized: String): Seq[Ability] = {
    serialized.map(
      _ match {
        case 'Q' => Ability.Q
        case 'W' => Ability.W
        case 'E' => Ability.E
        case 'R' => Ability.R
        case _ => Ability.U
      }
    )
  }

  def deserializeBuild(serialized: String): Seq[Int] = {
    serialized.split("\\|").map(_.toInt)
  }

}
