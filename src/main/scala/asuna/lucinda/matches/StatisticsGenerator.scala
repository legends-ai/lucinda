package asuna.lucinda.matches

import scala.util.{ Success, Try }

import asuna.common.legends.MatchSumHelpers._
import asuna.common.legends.MomentsHelpers._
import asuna.lucinda.statistics.StatisticsCombiner._
import asuna.proto.league.{ Ability, IntRange, MatchSum, Region, Role }
import asuna.proto.league.lucinda.{ AllChampionStatistics, MatchQuotient, Statistic }
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.lucinda.Statistics.Graphs.GoldPerTime
import cats.implicits._

object StatisticsGenerator {

  def makeStatistics(
    champions: Set[Int],
    allStats: AllChampionStatistics,
    patchNbhd: Map[String, AllChampionStatistics],
    roles: Set[Role],
    byRole: Map[Role, MatchSum],
    byPatch: Map[String, MatchSum],
    patches: Set[String]
  ): Statistics = {
    val roleStats = makeRoleStats(allStats, roles, byRole)

    // Then, we will fetch the stats for this patch for the champion.
    val roleKeys = if (roles.size == 0) {
      Set(roleStats.role)
    } else {
      roles
    }
    val patchStats = byRole.filterKeys(roleKeys).values.toList.combineAll

    // This is the quotient of the champion for the entire search space.
    val quot = QuotientGenerator.generate(patchStats)

    Statistics(
      roles = roleStats.some,
      scalars = quot.statistics.flatMap(_.scalars).map { scalars =>
        makeScalars(champions, allStats, scalars)
      },
      deltas = quot.statistics.flatMap(_.deltas).map { deltas =>
        makeDeltas(champions, allStats, deltas)
      },
      graphs = makeGraphs(allStats, patchNbhd, quot, champions).some,
      collections = quot.collections
    )
  }

  /**
    * Prepares the StatisticsRoles object.
    */
  private[this] def makeRoleStats(allStats: AllChampionStatistics, roles: Set[Role], roleSums: Map[Role, MatchSum]): Statistics.Roles = {
    // We get the total champions in role based on number of win rates in map.
    val totalChampionsInRole = allStats.results.flatMap(_.scalars)
      .map(_.wins.keys.filter(_ > 0).size).orEmpty

    // Number of games played by the champion for each role.
    val gamesByRole = roleSums.mapValues(
      _.statistics.map(_.plays).orEmpty).toMap

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

    // Gets the most picked role out of all roles passed in.
    def maxRole: Role = {
      val roleSpace = if (roles.size == 0) (Role.values.toSet - Role.UNDEFINED_ROLE) else roles
      roleStats
        .filter(stats => roleSpace(stats.role)).toSeq
        .sortBy(_.pickRate).map(_.role)
        .lastOption.getOrElse(Role.UNDEFINED_ROLE)
    }

    Statistics.Roles(
      // TODO(igm): return correct role
      role = maxRole,
      totalChampionsInRole = totalChampionsInRole,
      roleStats = roleStats.toSeq
    )
  }

  private def getStat[T](
    obj: Option[T],
    champions: Set[Int],
    accessor: T => Map[Int, Statistic],
    moments: Option[MatchQuotient.Statistics.Moments]
  ): Option[Statistic] = {
    // TODO(igm): support multiple champs
    val champion = champions.toSeq.headOption.getOrElse(0)
    obj.flatMap { statsMap =>
      accessor(statsMap).get(champion)
    }.map { baseStats =>
      if (moments.isDefined) {
        val moms = moments.get
        baseStats.copy(
          // in theory these should be the same, but in case they're not let's add it back
          mean = moms.mean,
          stdev = Math.sqrt(moms.variance),
          skewness = moms.skewness,
          kurtosis = moms.kurtosis
        )
      } else {
        baseStats
      }
    }.getOrElse(Statistic()).some
  }

  private def getDelta(
    deltas: Option[AllChampionStatistics.Results.Deltas],
    champions: Set[Int],
    accessor: AllChampionStatistics.Results.Deltas => Option[AllChampionStatistics.Results.Deltas.Delta],
    quot: Option[MatchQuotient.Statistics.Deltas.Delta]
  ): Option[Statistics.Deltas.Delta] = {
    val delta = deltas.flatMap(accessor)
    val get = getStat(
      delta, champions,
      _: AllChampionStatistics.Results.Deltas.Delta => Map[Int, Statistic],
      _: Option[MatchQuotient.Statistics.Moments]
    )
    if (!delta.isDefined) {
      return None
    }
    Statistics.Deltas.Delta(
      zeroToTen = get(_.zeroToTen, quot.flatMap(_.zeroToTen)),
      tenToTwenty = get(_.tenToTwenty, quot.flatMap(_.zeroToTen)),
      twentyToThirty = get(_.twentyToThirty, quot.flatMap(_.zeroToTen)),
      thirtyToEnd = get(_.thirtyToEnd, quot.flatMap(_.zeroToTen))
    ).some
  }

  def makeScalars(
    champions: Set[Int],
    roleStats: AllChampionStatistics,
    quot: MatchQuotient.Statistics.Scalars
  ): Statistics.Scalars = {
    val getScalar = getStat(
      roleStats.results.flatMap(_.scalars),
      champions,
      _: AllChampionStatistics.Results.Scalars => Map[Int, Statistic],
      _: Option[MatchQuotient.Statistics.Moments]
    )
    val derivatives = roleStats.results.flatMap(_.derivatives)

    // We calculate the pick rate stat out here, as we need it to find the gamesPlayed stat.
    val pickRateStat = getStat[AllChampionStatistics.Results.Derivatives](derivatives, champions, _.picks, None)

    // Number of games played by all champions
    val allGamesPlayed = roleStats.sums.flatMap(_.scalars).map(_.plays).orEmpty
    val meanGamesPlayed = allGamesPlayed.values.toList.combineAll.toDouble / allGamesPlayed.size

    // Number of games played by these champions in this role.
    val gamesPlayed = allGamesPlayed.filterKeys(champions).values.toList.combineAll

    // We can derive the gamesPlayed stat from the pickRate stat as they are virtually identical.
    val gamesPlayedStat = pickRateStat.map { stat =>
      stat
      // First, let's set the correct value for games played.
        .update(_.mean := gamesPlayed.toDouble)
        .update(_.meanAcrossRole := meanGamesPlayed)

      // All other attributes of stat (rank, change, champ) are the same, so we are done.
    }

    Statistics.Scalars(
      winRate = getScalar(_.wins, quot.wins),
      pickRate = pickRateStat,
      banRate = getStat[AllChampionStatistics.Results.Derivatives](derivatives, champions, _.bans, None),
      gamesPlayed = gamesPlayedStat,
      goldEarned = getScalar(_.goldEarned, quot.goldEarned),
      kills = getScalar(_.kills, quot.kills),
      deaths = getScalar(_.deaths, quot.deaths),
      assists = getScalar(_.assists, quot.assists),
      damageDealt = getScalar(_.damageDealt, quot.damageDealt),
      damageTaken = getScalar(_.damageTaken, quot.damageTaken),
      minionsKilled = getScalar(_.minionsKilled, quot.minionsKilled),
      teamJungleMinionsKilled = getScalar(_.teamJungleMinionsKilled, quot.teamJungleMinionsKilled),
      enemyJungleMinionsKilled = getScalar(_.enemyJungleMinionsKilled, quot.enemyJungleMinionsKilled),
      killingSpree = getScalar(_.killingSpree, quot.killingSpree),
      wardsBought = getScalar(_.wardsBought, quot.wardsBought),
      wardsPlaced = getScalar(_.wardsPlaced, quot.wardsPlaced),
      wardsKilled = getScalar(_.wardsKilled, quot.wardsKilled),
      crowdControl = getScalar(_.crowdControl, quot.crowdControl),
      firstBlood = getScalar(_.firstBlood, quot.firstBlood),
      firstBloodAssist = getScalar(_.firstBloodAssist, quot.firstBloodAssist),
      doublekills = getScalar(_.doublekills, quot.doublekills),
      triplekills = getScalar(_.triplekills, quot.triplekills),
      quadrakills = getScalar(_.quadrakills, quot.quadrakills),
      pentakills = getScalar(_.pentakills, quot.pentakills)
    )
  }

  def makeDeltas(
    champions: Set[Int],
    roleStats: AllChampionStatistics,
    quot: MatchQuotient.Statistics.Deltas
  ): Statistics.Deltas = {
    val getDeltas = getDelta(
      roleStats.results.flatMap(_.deltas),
      champions,
      _: AllChampionStatistics.Results.Deltas => Option[AllChampionStatistics.Results.Deltas.Delta],
      _: Option[MatchQuotient.Statistics.Deltas.Delta]
    )
    Statistics.Deltas(
      csDiff = getDeltas(_.csDiff, quot.csDiff),
      xpDiff = getDeltas(_.xpDiff, quot.xpDiff),
      damageTakenDiff = getDeltas(_.damageTakenDiff, quot.damageTakenDiff),
      xpPerMin = getDeltas(_.xpPerMin, quot.xpPerMin),
      goldPerMin = getDeltas(_.goldPerMin, quot.goldPerMin),
      towersPerMin = getDeltas(_.towersPerMin, quot.towersPerMin),
      wardsPlaced = getDeltas(_.wardsPlaced, quot.wardsPlaced),
      damageTaken = getDeltas(_.damageTaken, quot.damageTaken)
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
    champions: Set[Int]
  ): Statistics.Graphs = {
    val results = roleStats.results
    // TODO(igm): support multiple champions in graphs
    val id = champions.toSeq.headOption.getOrElse(-1)
    Statistics.Graphs(
      // Win/pick/ban distribution across all champions.
      distribution = Some(Statistics.Graphs.Distribution(
        winRate = results.flatMap(_.scalars).map(_.wins.mapValues(_.mean)).orEmpty,
        pickRate = results.flatMap(_.derivatives).map(_.picks.mapValues(_.mean)).orEmpty,
        banRate = results.flatMap(_.derivatives).map(_.bans.mapValues(_.mean)).orEmpty
      )),

      // Per-patch statistics.
      // TODO(igm): investigate why this code is so freaking ugly
      byPatch = patchStats.mapValues(_.results).map { case (patch, patchResults) =>
        Statistics.Graphs.ByPatch(
          patch = patch,
          winRate = patchResults.flatMap(_.scalars)
            .flatMap(_.wins.mapValues(_.mean).get(id)).orEmpty,
          pickRate = patchResults.flatMap(_.derivatives)
            .flatMap(_.picks.mapValues(_.mean).get(id)).orEmpty,
          banRate = patchResults.flatMap(_.derivatives)
            .flatMap(_.bans.mapValues(_.mean).get(id)).orEmpty
        )
      }.toSeq,

      byGameLength = quot.collections.map(_.durations).getOrElse(Map()).map { case (duration, stats) =>
        Statistics.Graphs.ByGameLength(
          gameLength = Some(IntRange(min = duration, max = duration)),
          winRate = stats.winRate
        )
      }.toSeq,

      physicalDamage = results.flatMap(_.scalars).flatMap(_.physicalDamage.get(id)).map(_.mean).orEmpty,
      magicDamage = results.flatMap(_.scalars).flatMap(_.magicDamage.get(id)).map(_.mean).orEmpty,
      trueDamage = results.flatMap(_.scalars).flatMap(_.trueDamage.get(id)).map(_.mean).orEmpty,

      goldOverTime = {
        val gpm = quot.statistics.flatMap(_.deltas).flatMap(_.goldPerMin)
        List(
          GoldPerTime(
            gold = gpm.flatMap(_.zeroToTen).map(_.mean).orEmpty,
            time = Some(IntRange(min = 0, max = 10))
          ),
          GoldPerTime(
            gold = gpm.flatMap(_.tenToTwenty).map(_.mean).orEmpty,
            time = Some(IntRange(min = 10, max = 20))
          ),
          GoldPerTime(
            gold = gpm.flatMap(_.twentyToThirty).map(_.mean).orEmpty,
            time = Some(IntRange(min = 20, max = 30))
          ),
          GoldPerTime(
            gold = gpm.flatMap(_.thirtyToEnd).map(_.mean).orEmpty,
            time = Some(IntRange(min = 30, max = 40))
          )
        )
      }
    )
  }

}
