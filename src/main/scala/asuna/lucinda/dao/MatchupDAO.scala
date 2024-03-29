package asuna.lucinda.dao

import asuna.proto.league.{ Queue, Region, Role, Tier }
import asuna.proto.league.lucinda.MatchupOverview
import asuna.proto.league.lucinda.rpc.Constraints
import cats.implicits._
import monix.eval.Task

class MatchupDAO(allChampionStatisticsDAO: AllChampionStatisticsDAO) {

  def getMatchupOverviews(
    allChampions: Set[Int],
    patches: Set[String],
    prevPatch: Option[String],

    champion: Int,
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    constraints: Constraints,
  ): Task[Seq[MatchupOverview]] = {
    for {
      // First, let's get our statistics against each enemy.
      championStatistics <- allChampionStatisticsDAO.compute(
        AllChampionStatisticsDAO.Key(
          allChampions = allChampions,
          prevPatch = prevPatch,
          tiers = tiers,
          patches = patches,
          regions = regions,
          roles = roles,
          queues = queues,
          enemies = Set(champion),
          reverse = true,
          constraints = constraints,
        )
      )
    } yield {
      // First, let's extract all of our maps of interest.
      val wins = championStatistics.results.flatMap(_.scalars).map(_.wins).getOrElse(Map())
      val picks = championStatistics.results.flatMap(_.derivatives).map(_.picks).getOrElse(Map())
      val plays = championStatistics.sums.map(_.plays).getOrElse(Map())

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
      }.filter(_.pickRate.map(_.mean).orEmpty >= constraints.minPickRate)
    }
  }

}
