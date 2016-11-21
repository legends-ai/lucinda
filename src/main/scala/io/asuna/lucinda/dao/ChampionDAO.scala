package io.asuna.lucinda.dao

import io.asuna.proto.charon.CharonData.Static
import io.asuna.proto.lucinda.LucindaData.Champion
import scalaz.Scalaz._
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

  def getFull(
    tiers: Option[TierRange], patches: Option[PatchRange], champion: Int, region: Region,
    role: Role, minPlayRate: Double, enemy: Int = -1
  ): Future[Champion] = {
    val context = VulgateData.Context().some // TODO(igm): implement
    for {
      // Initial vulgate request.
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = context,
          patches = patches,
          tiers = tiers
        )
      )

      // The match aggregate. Our main honcho.
      // Contains all data that matters.
      matchAggregate <- matchAggregateDAO.get(
        factors.champions.toSet, factors.patches.toSet, factors.lastFivePatches.toSet,
        champion, factors.tiers.toSet, region,
        role, enemy, minPlayRate
      )

      // Matchup stuff. This is very expensive but fortunately it's cached.
      enemyStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, enemy)
      championStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, enemy, reverse = true)

      // Vulgate champion data
      champions <- vulgate.getChampions(
        VulgateRpc.GetChampionsRequest(
          context = context,

          // List of all champions we care about.
          // In theory this list will also include the champion requesting the data.
          champions = enemyStatistics.sums.flatMap(_.scalars).map(_.plays.keys).getOrElse(Seq()).toSeq
        )
      )
    } yield {
      Champion(
        metadata = Champion.Metadata(
          staticInfo = champions.champions.get(champion)
            // TODO(igm): patch start and end
        ).some,
        matchAggregate = matchAggregate.some,
        matchups = makeMatchups(enemyStatistics, championStatistics)
      )
    }
  }

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

  private def makeMatchups(enemy: ChampionStatistics, champion: ChampionStatistics): Seq[MatchupOverview] = {
    // TODO(igm): implement
    return Seq()
  }

}
