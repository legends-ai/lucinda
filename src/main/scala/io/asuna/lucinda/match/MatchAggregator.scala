package io.asuna.lucinda.statistics

import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics
import io.asuna.proto.match_aggregate.MatchAggregateRoles
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.match_aggregate.MatchAggregate

object MatchAggregator {

  /**
    * Fetches and aggregates information about a single champion.
    *
    * @param patches -- The patches in the patch range we are considering.
    * @param lastFivePatches -- The last five patches of the game.
    */
  def aggregate(
    champion: Int, champions: Set[Int], patches: Set[String],
    lastFivePatches: Set[String],
    tiers: Set[Int], region: Region, role: Role, enemy: Int = -1,
    minPlayRate: Double
  )(implicit db: LucindaDatabase, ec: ExecutionContext): Future[MatchAggregate] = {
    // First, let's retrieve all stats for this combination.
    // TODO(igm): use the cache when it is implemented
    val allStats = StatisticsAggregator.aggregate(champions, patches, tiers, region, enemy)

    // Next, let's get per-role sums.
    val byRoleFilters = Role.values.map { someRole =>
      (role, patches.flatMap {
         patch => buildFilters(champion, patch, tiers, region, enemy, someRole)
       })
    }.toMap
    val byRole = byRoleFilters.mapValues(filters => db.matchSums.sum(filters))

    // Next, let's get per-patch sums.
    val byPatchFilters = lastFivePatches.map { patch =>
      (role, buildFilters(champion, patch, tiers, region, enemy, role))
    }.toMap
    val byPatch = byPatchFilters.mapValues(filters => db.matchSums.sum(filters))

    // Finally, we'll build everything.
    MatchAggregate(
      role = Option(makeRoleStats(role, champion, allStats))
    )
  }

  /**
    * Build filters for the given champion with the given tiers.
    */
  private def buildFilters(champion: Int, patch: String, tiers: Set[Int], region: Region, enemy: Int = -1, role: Role): Set[MatchFilters] = {
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
  private def makeRoleStats(role: Role, champion: Int, allStats: ChampionStatistics): MatchAggregateRoles = {
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
      MatchAggregateRoles.RoleStats(
        role = role,
        pickRate = numMatches.toDouble / totalGames,
        numMatches = numMatches.toInt
      )
    }

    MatchAggregateRoles(
      role = role,
      totalChampionsInRole = totalChampionsInRole,
      roleStats = roleStats
    )
  }

}
