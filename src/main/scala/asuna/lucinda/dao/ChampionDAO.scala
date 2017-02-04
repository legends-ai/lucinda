package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.immutable.Vector

import asuna.lucinda.VulgateHelpers
import asuna.lucinda.matches.MatchAggregator
import asuna.proto.league.{ PatchRange, QueueType, Region, Role, TierRange }
import asuna.proto.league.charon.static
import asuna.proto.league.lucinda.{ Champion, AllChampionStatistics, Matchup, MatchupOverview }
import asuna.proto.league.vulgate.AggregationFactors
import asuna.proto.league.vulgate.VulgateGrpc.Vulgate
import cats.implicits._

class ChampionDAO(
  vulgate: Vulgate, statisticsDAO: AllChampionStatisticsDAO, matchAggregateDAO: MatchAggregateDAO
)(implicit ec: ExecutionContext) {

  def getMatchupOverviews(
    factors: AggregationFactors,
    champion: Option[Int],
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
