package io.asuna.lucinda.dao

import io.asuna.proto.enums.{ Region, Role }
import io.asuna.proto.lucinda.LucindaData.Champion
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

}
