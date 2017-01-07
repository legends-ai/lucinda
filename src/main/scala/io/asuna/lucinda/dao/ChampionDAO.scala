package io.asuna.lucinda.dao

import io.asuna.lucinda.VulgateHelpers
import io.asuna.lucinda.matches.MatchAggregator
import io.asuna.proto.charon.CharonData.{ Static }
import io.asuna.proto.enums.QueueType
import io.asuna.proto.lucinda.LucindaData.{ Champion, Matchup }
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.{ Champion, ChampionStatistics }
import io.asuna.proto.range.{ PatchRange, TierRange }
import io.asuna.proto.service_vulgate.VulgateGrpc.Vulgate
import io.asuna.proto.service_vulgate.VulgateRpc
import io.asuna.proto.vulgate.VulgateData
import scala.concurrent.{ ExecutionContext, Future }

class ChampionDAO(
  vulgate: Vulgate, statisticsDAO: ChampionStatisticsDAO, matchAggregateDAO: MatchAggregateDAO
)(implicit ec: ExecutionContext) {

  /**
    * Gets a Champion.
    */
  def getChampion(
    factors: AggregationFactors,
    champion: Int,
    region: Region,
    role: Role,
    queues: Set[QueueType],
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[Champion] = {
    getWithoutMatchups(
      factors, champion, region,
      role, queues, minPlayRate, -1,
      forceRefresh = forceRefresh
    )
  }

  /**
    *  Gets a Matchup.
    */
  def getMatchup(
    factors: AggregationFactors, focus: Int, region: Region,
    role: Role, queues: Set[QueueType], minPlayRate: Double, enemy: Int, forceRefresh: Boolean = false
  ): Future[Matchup] = {
    for {
      focusChamp <- getWithoutMatchups(
        factors, focus, region, role, queues, minPlayRate, enemy, forceRefresh = forceRefresh)
      enemyChamp <- getWithoutMatchups(
        factors, enemy, region, role, queues, minPlayRate, focus, forceRefresh = forceRefresh)
    } yield Matchup(focus = Some(focusChamp), enemy = Some(enemyChamp))
  }

  private def getWithoutMatchups(
    factors: AggregationFactors,
    champion: Int,
    region: Region,
    role: Role,
    queues: Set[QueueType],
    minPlayRate: Double,
    enemy: Int,
    forceRefresh: Boolean = false
  ): Future[Champion] = {
    // TODO(igm): locale
    for {
      // The match aggregate. Our main honcho.
      // Contains all data that matters.
      matchAggregate <- matchAggregateDAO.get(
        champions = factors.champions.toSet,
        patches = factors.patches.toSet,
        lastFivePatches = factors.lastFivePatches.toList,
        prevPatches = factors.prevPatches,
        champion = champion,
        tiers = factors.tiers.toSet,
        region = region,
        role = role,
        enemy = enemy,
        queues = queues,
        minPlayRate = minPlayRate,
        forceRefresh = forceRefresh
      )
    } yield Champion(
      id = champion,
      matchAggregate = Some(matchAggregate)
    )
  }

  private def getChamps(statistics: ChampionStatistics): Set[Int] =
    statistics.sums.flatMap(_.scalars).map(_.wins.keys.toSet).getOrElse(Set())

}
