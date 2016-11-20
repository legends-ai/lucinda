package io.asuna.lucinda.dao

import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion.MatchupOverview
import io.asuna.proto.lucinda.LucindaData.{ Champion, ChampionStatistics }
import scala.concurrent.{ ExecutionContext, Future }

class ChampionDAO(statisticsDAO: ChampionStatisticsDAO, matchAggregateDAO: MatchAggregateDAO)(implicit ec: ExecutionContext) {

  def get(
    champions: Set[Int], patches: Set[String], lastFivePatches: Set[String],
    champion: Int, tiers: Set[Int], region: Region,
    role: Role, enemy: Int = -1, minPlayRate: Double = 0.05
  ): Future[Champion] = {
    for {
      matchAggregate <- matchAggregateDAO.get(
        champions, patches, lastFivePatches,
        champion, tiers, region,
        role, enemy, minPlayRate
      )
    } yield Champion(
      matchAggregate = Some(matchAggregate)
    )
  }

  def getWithMatchups(
    champions: Set[Int], patches: Set[String], lastFivePatches: Set[String],
    champion: Int, tiers: Set[Int], region: Region,
    role: Role, enemy: Int = -1, minPlayRate: Double = 0.05
  ): Future[Champion] = {
    for {
      resChampion <- get(
        champions, patches, lastFivePatches,
        champion, tiers, region,
        role, enemy, minPlayRate
      )
      enemyStatistics <- statisticsDAO.getForPatches(champions, tiers, patches, region, role, enemy)
      championStatistics <- statisticsDAO.getForPatches(champions, tiers, patches, region, role, enemy, reverse = true)
    } yield {
      Champion(
        matchAggregate = resChampion.matchAggregate,
        matchups = makeMatchups(enemyStatistics, championStatistics)
      )
    }
  }

  private def makeMatchups(enemy: ChampionStatistics, champion: ChampionStatistics): Seq[MatchupOverview] = {
    // TODO(igm): implement
    return Seq()
  }

}
