package io.asuna.lucinda.dao

import io.asuna.lucinda.FutureUtil
import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.lucinda.matches.MatchAggregator
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion.MatchAggregate
import scala.concurrent.{ ExecutionContext, Future }

class MatchAggregateDAO(db: LucindaDatabase, statistics: ChampionStatisticsDAO)(implicit ec: ExecutionContext) {

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
  ): Future[MatchAggregate] = {
    // First, let's retrieve all stats for this combination.
    // TODO(igm): use the cache when it is implemented
    val allStatsFuts = patches.map { patch =>
      statistics.get(champions, tiers, patch, region, enemy).map((patch, _))
    }

    // This future contains an element of the form Map[String, ChampionStatistics]
    // where key is the patch and value is the stats.
    val allStatsFut = Future.sequence(allStatsFuts).map(_.toMap)

    // Next, let's get per-role sums.
    val byRoleFilters = Role.values.map { someRole =>
      (role, patches.flatMap {
         patch => MatchAggregator.buildFilters(champion, patch, tiers, region, enemy, someRole)
       })
    }.toMap
    val byRoleFut = FutureUtil.sequenceMap(byRoleFilters.mapValues(filters => db.matchSums.sum(filters)))

    // Next, let's get per-patch sums.
    val byPatchFilters = lastFivePatches.map { patch =>
      (patch, MatchAggregator.buildFilters(champion, patch, tiers, region, enemy, role))
    }.toMap
    val byPatchFut = FutureUtil.sequenceMap(byPatchFilters.mapValues(filters => db.matchSums.sum(filters)))

    // Finally, we'll execute and build everything.
    for {
      allStats <- allStatsFut
      byRole <- byRoleFut
      byPatch <- byPatchFut
    } yield MatchAggregator.makeAggregate(role, champion, minPlayRate, allStats, byRole, byPatch)
  }

}
