package io.asuna.lucinda.dao

import io.asuna.lucinda.matches.MatchAggregator
import io.asuna.proto.charon.CharonData.{ Static }
import io.asuna.proto.lucinda.LucindaData.{ Champion, Matchup }
import io.asuna.proto.vulgate.VulgateData.AggregationFactors
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

  /**
    * Gets a Champion.
    */
  def getChampion(
    tiers: Option[TierRange], patches: Option[PatchRange], champion: Int, region: Region,
    role: Role, minPlayRate: Double
  ): Future[Champion] = {
    val release = patches match {
      case Some(range) => VulgateData.Context.Release.Patch(range.max)
      case None => VulgateData.Context.Release.Empty
    }
    val context = VulgateData.Context(release=release).some // TODO(igm): implement
    for {
      (factors, bareChamp) <- getWithoutMatchups(tiers, patches, champion, region, role, minPlayRate, -1)

      // Matchup stuff. This is very expensive but fortunately it's cached.
      enemyStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, -1)
      championStatistics <- statisticsDAO.getForPatches(
        factors.champions.toSet, factors.tiers.toSet, factors.patches.toSet, region, role, -1, reverse = true)

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
      bareChamp.copy(matchups = makeMatchups(champions.champions, enemyStatistics, championStatistics))
    }
  }

  /**
    *  Gets a Matchup.
    */
  def getMatchup(
    tiers: Option[TierRange], patches: Option[PatchRange], focus: Int, region: Region,
    role: Role, minPlayRate: Double, enemy: Int
  ): Future[Matchup] = {
    for {
      (_, focusChamp) <- getWithoutMatchups(tiers, patches, focus, region, role, minPlayRate, enemy)
      (_, enemyChamp) <- getWithoutMatchups(tiers, patches, enemy, region, role, minPlayRate, focus)
    } yield Matchup(focus = focusChamp.some, enemy = enemyChamp.some)
  }

  private def getWithoutMatchups(
    tiers: Option[TierRange], patches: Option[PatchRange], champion: Int, region: Region,
    role: Role, minPlayRate: Double, enemy: Int = -1
  ): Future[(AggregationFactors, Champion)] = {
    val context = VulgateData.Context().some // TODO(igm): implement
    for {
      // Initial vulgate request.
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = context,
          patches = patches,
          tiers = tiers,
          champion = champion
        )
      )

      // The match aggregate. Our main honcho.
      // Contains all data that matters.
      matchAggregate <- matchAggregateDAO.get(
        factors.champions.toSet, factors.patches.toSet, factors.lastFivePatches.toSet,
        champion, factors.tiers.toSet, region,
        role, enemy, minPlayRate
      )
    } yield {
      (
        factors,
        Champion(
          metadata = Champion.Metadata(
            staticInfo = factors.focus
              // TODO(igm): patch start and end
          ).some,
          matchAggregate = matchAggregate.some
        )
      )
    }
  }

  private def makeMatchups(championData: Map[Int, Static.Champion], enemy: ChampionStatistics, focus: ChampionStatistics): Seq[MatchupOverview] = {
    // First, let's get all of the common champions in both data sets.
    val champs = getChamps(enemy) intersect getChamps(focus)

    // Now, let's build our result by iterating over this.
    champs.map { champ =>
      MatchupOverview(
        enemyData = championData.get(champ),
        focusStatistics = MatchAggregator.makeStatistics(champ, focus).some,
        enemyStatistics = MatchAggregator.makeStatistics(champ, enemy).some
      )
    }.toSeq
  }

  private def getChamps(statistics: ChampionStatistics): Set[Int] =
    statistics.sums.flatMap(_.scalars).map(_.wins.keys.toSet).getOrElse(Set())

}
