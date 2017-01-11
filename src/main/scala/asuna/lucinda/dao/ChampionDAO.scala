package asuna.lucinda.dao

import asuna.proto.ids.ChampionId
import cats.implicits._
import asuna.lucinda.VulgateHelpers
import asuna.lucinda.matches.MatchAggregator
import asuna.proto.charon.CharonData.{ Static }
import asuna.proto.enums.QueueType
import asuna.proto.lucinda.LucindaData.{ Champion, Matchup, MatchupOverview }
import asuna.proto.vulgate.VulgateData.AggregationFactors
import asuna.proto.enums.{ Region, Role }
import asuna.proto.lucinda.LucindaData.{ Champion, ChampionStatistics }
import asuna.proto.range.{ PatchRange, TierRange }
import asuna.proto.service_vulgate.VulgateGrpc.Vulgate
import asuna.proto.service_vulgate.VulgateRpc
import asuna.proto.vulgate.VulgateData
import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.immutable.Vector

class ChampionDAO(
  vulgate: Vulgate, statisticsDAO: ChampionStatisticsDAO, matchAggregateDAO: MatchAggregateDAO
)(implicit ec: ExecutionContext) {

  /**
    *  Gets a Matchup.
    */
  def getMatchup(
    factors: AggregationFactors,
    focus: Option[ChampionId],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    minPlayRate: Double,
    enemy: Option[ChampionId],
    forceRefresh: Boolean
  ): Future[Matchup] = {
    for {
      focusChamp <- getChampion(
        factors, focus, region, role, queues, minPlayRate, enemy, forceRefresh = forceRefresh)
      enemyChamp <- getChampion(
        factors, enemy, region, role, queues, minPlayRate, focus, forceRefresh = forceRefresh)
    } yield Matchup(focus = Some(focusChamp), enemy = Some(enemyChamp))
  }

  def getChampion(
    factors: AggregationFactors,
    champion: Option[ChampionId],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    minPlayRate: Double,
    enemy: Option[ChampionId],
    forceRefresh: Boolean
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
      id = champion.map(_.value).orEmpty,
      matchAggregate = Some(matchAggregate)
    )
  }

  private def getChamps(statistics: ChampionStatistics): Set[Int] =
    statistics.sums.flatMap(_.scalars).map(_.wins.keys.toSet).getOrElse(Set())

  /**
    * Gets a Champion.
    */
  def getMatchupOverviews(
    factors: AggregationFactors,
    champion: Option[ChampionId],
    region: Region,
    role: Role,
    queues: Set[QueueType],
    minPlayRate: Double,
    forceRefresh: Boolean = false
  ): Future[Vector[MatchupOverview]] = {
    for {
      // First, let's get our statistics against each enemy.
      championStatistics <- statisticsDAO.get(
        factors = factors,
        region = region,
        role = role,
        enemy = champion,
        queues = queues,
        reverse = true,
        forceRefresh = forceRefresh
      )
    } yield {
      // First, let's extract all of our maps of interest.
      val wins = championStatistics.results.flatMap(_.scalars).map(_.wins).getOrElse(Map())
      val picks = championStatistics.results.flatMap(_.derivatives).map(_.picks).getOrElse(Map())
      val plays = championStatistics.sums.flatMap(_.scalars).map(_.plays).getOrElse(Map())

      // Let's get all champs that have values for all three
      val commonChamps = wins.keys.toSet intersect picks.keys.toSet intersect plays.keys.toSet

      // Now, let's construct all of the MatchupOverviews.
      commonChamps.toVector.map { enemy =>
        // None of these should ever throw exceptions for not getting the option.
        MatchupOverview(
          enemy = enemy,
          pickRate = picks.get(enemy),
          winRate = wins.get(enemy),
          plays = plays.get(enemy).orEmpty.toInt
        )

      // Check that we obey the minimum play rate
      }.filter(_.pickRate.map(_.value).orEmpty >= minPlayRate)
    }
  }

}
