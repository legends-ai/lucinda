package io.asuna.lucinda.matches

import io.asuna.asunasan.legends.MatchSumOperators._
import io.asuna.lucinda.statistics.StatisticsCombiner
import io.asuna.proto.enums.{Ability, Region, Role}
import io.asuna.proto.lucinda.LucindaData.Champion.MatchAggregate
import io.asuna.proto.lucinda.LucindaData.{Statistic, ChampionStatistics}
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.match_quotient.MatchQuotient
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.range.IntRange

object MatchAggregator {

  def makeAggregate(
    role: Role, champion: Int, minPlayRate: Double,
    allStats: Map[String, ChampionStatistics], byRole: Map[Role, MatchSum], byPatch: Map[String, MatchSum]
  ): MatchAggregate = {
    val combinedStats = StatisticsCombiner.combineMulti(allStats.values)
    val roleStats = combinedStats.statistics.find(_.role == role)

    // Stats of a role by patch. This maps a patch to the ChampionStatistics object for the patch.
    val roleStatsByPatch = allStats
      .mapValues(_.statistics.find(_.role == role).getOrElse(ChampionStatistics.Statistics()))

    // This is the quotient of the champion for the entire search space.
    val quot = QuotientGenerator.generate(byPatch.values.foldLeft(MatchSum())(_ + _))

    MatchAggregate(
      roles = Option(makeRoleStats(role, champion, combinedStats)),
      statistics = Option(makeStatistics(role, champion, combinedStats)),
      graphs = for {
        roles <- roleStats
      } yield makeGraphs(roles, roleStatsByPatch, quot, champion),
      collections = Option(makeCollections(quot, minPlayRate))
    )
  }

  /**
    * Build filters for the given champion with the given tiers.
    */
  def buildFilters(champion: Int, patch: String, tiers: Set[Int], region: Region, enemy: Int = -1, role: Role): Set[MatchFilters] = {
    for {
      tier <- tiers
    } yield MatchFilters(
      championId = champion,
      patch = patch,
      tier = tier,
      region = region,
      enemyId = enemy,
      role = role
    )
  }

  /**
    * Prepares the MatchAggregateRoles object.
    */
  private def makeRoleStats(role: Role, champion: Int, allStats: ChampionStatistics): MatchAggregate.Roles = {
    val myRoleStats = allStats.statistics.find(_.role == role)

    // We get the total champions in role based on number of win rates in map.
    val totalChampionsInRole = myRoleStats
      .flatMap(_.results).flatMap(_.scalars)
      .map(_.wins).map(_.size).getOrElse(0)

    // Number of games played by the champion for each role.
    val gamesByRole = allStats.statistics.map { case stats =>
      (stats.role, stats.sums.flatMap(_.scalars).flatMap(_.plays.get(champion)).getOrElse(0L))
    }

    // Number of total games played by the champion.
    val totalGames = gamesByRole.map { case (_, plays) => plays }.sum

    // Stats by role.
    val roleStats = gamesByRole.map { case (role, numMatches) =>
      MatchAggregate.Roles.RoleStats(
        role = role,
        pickRate = numMatches.toDouble / totalGames,
        numMatches = numMatches.toInt
      )
    }

    MatchAggregate.Roles(
      role = role,
      totalChampionsInRole = totalChampionsInRole,
      roleStats = roleStats
    )
  }

  private def getStat[T](obj: Option[T], champion: Int, accessor: T => Map[Int, Statistic]): Option[Statistic] = {
    obj.flatMap(accessor(_).get(champion))
  }

  private def getDelta(deltas: Option[ChampionStatistics.Results.Deltas], champion: Int, accessor: ChampionStatistics.Results.Deltas => Option[ChampionStatistics.Results.Deltas.Delta]): Option[MatchAggregate.Statistics.Deltas.Delta] = {
    val delta = deltas.flatMap(accessor)
    val get = getStat(delta, champion, _: ChampionStatistics.Results.Deltas.Delta => Map[Int, Statistic])
    if (!delta.isDefined) {
      return None
    }
    Option(MatchAggregate.Statistics.Deltas.Delta(
      zeroToTen = get(_.zeroToTen),
      tenToTwenty = get(_.tenToTwenty),
      twentyToThirty = get(_.twentyToThirty),
      thirtyToEnd = get(_.thirtyToEnd)
    ))
  }

  private def makeStatistics(role: Role, champion: Int, allStats: ChampionStatistics): MatchAggregate.Statistics = {
    val roleStats = allStats.statistics.find(_.role == role)
    val results = roleStats.flatMap(_.results)

    val scalars = results.flatMap(_.scalars)
    val getScalar = getStat(scalars, champion, _: ChampionStatistics.Results.Scalars => Map[Int, Statistic])
    val derivatives = results.flatMap(_.derivatives)

    // We calculate the pick rate stat out here, as we need it to find the gamesPlayed stat.
    val pickRateStat = getStat[ChampionStatistics.Results.Derivatives](derivatives, champion, _.picks)

    // Number of games played by this champion in this role.
    val gamesPlayed = roleStats
      .flatMap(_.sums).flatMap(_.scalars).flatMap(_.plays.get(champion)).getOrElse(0L)

    // We can derive the gamesPlayed stat from the pickRate stat as they are virtually identical.
    val gamesPlayedStat = pickRateStat.map { stat =>
      stat
      // First, let's set the correct value for games played.
        .update(_.value := gamesPlayed.toDouble)
      // The average can be derived by finding the multiplier of the value.
        .update(_.average := ((stat.average / stat.value) * gamesPlayed.toDouble).floor)

      // All other attributes of stat (rank, change, champ) are the same, so we are done.
    }

    val scalarsStats = MatchAggregate.Statistics.Scalars(
      winRate = getScalar(_.wins),
      pickRate = pickRateStat,
      banRate = getStat[ChampionStatistics.Results.Derivatives](derivatives, champion, _.bans),
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

    val deltas = results.flatMap(_.deltas)
    val getDeltas = getDelta(deltas, champion, _: ChampionStatistics.Results.Deltas => Option[ChampionStatistics.Results.Deltas.Delta])
    val deltasStats = MatchAggregate.Statistics.Deltas(
      csDiff = getDeltas(_.csDiff),
      xpDiff = getDeltas(_.xpDiff),
      damageTakenDiff = getDeltas(_.damageTakenDiff),
      xpPerMin = getDeltas(_.xpPerMin),
      goldPerMin = getDeltas(_.goldPerMin),
      towersPerMin = getDeltas(_.towersPerMin),
      wardsPlaced = getDeltas(_.wardsPlaced),
      damageTaken = getDeltas(_.damageTaken)
    )

    MatchAggregate.Statistics(
      scalars = Option(scalarsStats),
      deltas = Option(deltasStats)
    )
  }

  /**
    * Makes our graphs.
    *
    * @param patchStats: Map of patch to results for the role.
    */
  def makeGraphs(
    roleStats: ChampionStatistics.Statistics,
    patchStats: Map[String, ChampionStatistics.Statistics],
    quot: MatchQuotient,
    id: Int
  ): MatchAggregate.Graphs = {
    val results = roleStats.results
    MatchAggregate.Graphs(
      // Win/pick/ban distribution across all champions.
      distribution = Option(MatchAggregate.Graphs.Distribution(
        winRate = results.flatMap(_.scalars).map(_.wins.mapValues(_.value)).get,
        pickRate = results.flatMap(_.derivatives).map(_.picks.mapValues(_.value)).get,
        banRate = results.flatMap(_.derivatives).map(_.bans.mapValues(_.value)).get
      )),

      // Per-patch statistics.
      // TODO(igm): investigate why this code is so freaking ugly
      byPatch = patchStats.mapValues(_.results).map { case (patch, patchResults) =>
        MatchAggregate.Graphs.ByPatch(
          patch = patch,
          winRate = patchResults.flatMap(_.scalars).flatMap(_.wins.mapValues(_.value).get(id)).get,
          pickRate = patchResults.flatMap(_.derivatives).flatMap(_.picks.mapValues(_.value).get(id)).get,
          banRate = patchResults.flatMap(_.derivatives).flatMap(_.bans.mapValues(_.value).get(id)).get
        )
      }.toSeq,

      byGameLength = quot.durations.map { case (duration, stats) =>
        MatchAggregate.Graphs.ByGameLength(
          gameLength = Option(IntRange(min = duration, max = duration)),
          winRate = stats.wins
        )
      }.toSeq
    )
  }

  /**
    * Makes our collections.
    */
  def makeCollections(quot: MatchQuotient, minPlayRate: Double): MatchAggregate.Collections = {
    MatchAggregate.Collections(
      runes = quot.runes
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (runeSet, subscalars) =>
          MatchAggregate.Collections.RuneSet(
            runes = deserializeBonusSet(runeSet),
            subscalars = Option(subscalars)
          )
      }.toSeq,

      masteries = quot.masteries
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (masterySet, subscalars) =>
          MatchAggregate.Collections.MasterySet(
            masteries = deserializeBonusSet(masterySet),
            subscalars = Option(subscalars)
          )
      }.toSeq,

      summonerSpells = quot.summoners
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (spells, subscalars) =>
          val (spell1, spell2) = deserializeSummoners(spells)
          MatchAggregate.Collections.SummonerSet(
            spell1 = spell1,
            spell2 = spell2,
            subscalars = Option(subscalars)
          )
      }.toSeq,

      skillOrders = quot.skillOrders
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (skillOrder, subscalars) =>
          MatchAggregate.Collections.SkillOrder(
            skillOrder = deserializeSkillOrder(skillOrder),
            subscalars = Option(subscalars)
          )
      }.toSeq,

      starterItems = quot.starterItems
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (build, subscalars) =>
          MatchAggregate.Collections.Build(
            build = deserializeBuild(build),
            subscalars = Option(subscalars)
          )
      }.toSeq,

      buildPath = quot.buildPath
        .filter(_._2.plays >= minPlayRate)  // Ensure minimum play rate is met
        .map { case (build, subscalars) =>
          MatchAggregate.Collections.Build(
            build = deserializeBuild(build),
            subscalars = Option(subscalars)
          )
      }.toSeq
    )
  }

  def deserializeBonusSet(serialized: String): Map[Int, Int] = {
    serialized.split("|").map { part =>
      var element = part.split(":").map(_.toInt)
      (element(0), element(1))
    }.toMap
  }

  def deserializeSummoners(serialized: String): (Int, Int) = {
    val Array(a, b) = serialized.split("|")
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
    serialized.split("|").map(_.toInt)
  }

}
