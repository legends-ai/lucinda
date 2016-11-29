package io.asuna.lucinda.dao

import io.asuna.lucinda.VulgateHelpers
import io.asuna.lucinda.matches.MatchAggregator
import io.asuna.proto.charon.CharonData.{ Static }
import io.asuna.proto.lucinda.LucindaData.{ Champion, Matchup }
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion.MatchupOverview
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
    factors: AggregationFactors, champion: Int, region: Region,
    role: Role, minPlayRate: Double, forceRefresh: Boolean = false
  ): Future[Champion] = {
    for {
      bareChamp <- getWithoutMatchups(factors, champion, region, role, minPlayRate, -1, forceRefresh = forceRefresh)

      // Matchup stuff. This is very expensive but fortunately it's cached.
      enemyStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, -1, forceRefresh = forceRefresh)
      championStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, -1, reverse = true, forceRefresh = forceRefresh)

      // Vulgate champion data
      champions <- vulgate.getChampions(
        VulgateRpc.GetChampionsRequest(
          // TODO(igm): locale
          context = Some(VulgateHelpers.makeVulgateContextOfPatch(factors.patches.last, region)),

          // List of all champions we care about.
          // In theory this list will also include the champion requesting the data.
          champions = enemyStatistics.sums.flatMap(_.scalars).map(_.plays.keys).getOrElse(Seq()).toSeq
        )
      )
    } yield {
      bareChamp.copy(matchups = makeMatchups(champions.champions, enemyStatistics, championStatistics))
    }
  }

  /**
    *  Gets a Matchup.
    */
  def getMatchup(
    factors: AggregationFactors, focus: Int, region: Region,
    role: Role, minPlayRate: Double, enemy: Int, forceRefresh: Boolean = false
  ): Future[Matchup] = {
    for {
      focusChamp <- getWithoutMatchups(factors, focus, region, role, minPlayRate, enemy, forceRefresh = forceRefresh)
      enemyChamp <- getWithoutMatchups(factors, enemy, region, role, minPlayRate, focus, forceRefresh = forceRefresh)
    } yield Matchup(focus = Some(focusChamp), enemy = Some(enemyChamp))
  }

  private def getWithoutMatchups(
    factors: AggregationFactors, champion: Int, region: Region,
    role: Role, minPlayRate: Double, enemy: Int = -1, forceRefresh: Boolean = false
  ): Future[Champion] = {
    // TODO(igm): locale
    for {
      // The match aggregate. Our main honcho.
      // Contains all data that matters.
      matchAggregate <- matchAggregateDAO.get(
        factors.champions.toSet, factors.patches.toSet, factors.lastFivePatches.toSet,
        champion, factors.tiers.toSet, region,
        role, enemy, minPlayRate, forceRefresh = forceRefresh
      )

      champ = Champion(
        id = champion,
        matchAggregate = Some(matchAggregate)
      )
    } yield champ
  }

  private def makeMatchups(championData: Map[Int, Static.Champion], enemy: ChampionStatistics, focus: ChampionStatistics): Seq[MatchupOverview] = {
    // First, let's get all of the common champions in both data sets.
    val champs = getChamps(enemy) intersect getChamps(focus)

    // Now, let's build our result by iterating over this.
    champs.map { champ =>
      MatchupOverview(
        enemyId = champ,
        focusStatistics = Some(MatchAggregator.makeStatistics(champ, focus)),
        enemyStatistics = Some(MatchAggregator.makeStatistics(champ, enemy))
      )
    }.toSeq
  }

  private def getChamps(statistics: ChampionStatistics): Set[Int] =
    statistics.sums.flatMap(_.scalars).map(_.wins.keys.toSet).getOrElse(Set())

}
