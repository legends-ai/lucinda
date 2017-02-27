package asuna.lucinda.dao

import scala.concurrent.{ ExecutionContext, Future }

import asuna.proto.league.{ Queue, Region, Role, Tier }
import asuna.proto.league.lucinda.MatchupOverview
import cats.implicits._

class MatchupDAO(
  allChampionStatisticsDAO: AllChampionStatisticsDAO
)(implicit ec: ExecutionContext) {

  def getMatchupOverviews(
    allChampions: Set[Int],
    patches: Set[String],
    prevPatch: Option[String],

    champion: Int,
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    minPickRate: Double
  ): Future[Vector[MatchupOverview]] = {
    for {
      // First, let's get our statistics against each enemy.
      championStatistics <- allChampionStatisticsDAO.get(
        allChampions = allChampions,
        prevPatch = prevPatch,
        tiers = tiers,
        patches = patches,
        regions = regions,
        roles = roles,
        queues = queues,
        enemies = Set(champion),
        reverse = true
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
      }.filter(_.pickRate.map(_.value).orEmpty >= minPickRate)
    }
  }

}
