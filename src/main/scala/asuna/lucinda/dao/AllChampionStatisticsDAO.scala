package asuna.lucinda.dao

import asuna.lucinda.statistics.ChangeMarker
import monix.eval.Task
import asuna.lucinda.statistics.FilterChampionsHelpers._
import asuna.proto.league._
import asuna.proto.league.lucinda.AllChampionStatistics
import cats.implicits._
import monix.cats._

class AllChampionStatisticsDAO(bareDAO: BareAllChampionStatisticsDAO) {

  /**
   * Fetches a AllChampionStatistics.Results object.
   */
  def getResults(
    allChampions: Set[Int],
    prevPatch: Option[String],
    patches: Set[String],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    minPickRate: Double = 0
  ): Task[AllChampionStatistics.Results] = {
    for {
      statistics <- get(
        allChampions = allChampions,
        patches = patches,
        prevPatch = prevPatch,
        tiers = tiers,
        regions = regions,
        roles = roles,
        queues = queues,
        enemies = enemies,
        reverse = false
      )
    } yield {
      // Get the results object
      val results = statistics.results.getOrElse(AllChampionStatistics.Results())

      // Find the champions that satisfy the min play rate
      val pickRates = results.derivatives
        .map(_.picks.mapValues(_.mean)).orEmpty
      val champs = pickRates.filter {
        case (champ, pickRate) => pickRate >= minPickRate
      }.keys

      // Filter maps for keys that contain the champion
      results.filterChampions(champs.toSet)
    }
  }

  /**
    * Gets a AllChampionStatistics object with caching.
    */
  def get(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    prevPatch: Option[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ): Task[AllChampionStatistics] = {
    prevPatch match {
      case Some(patch) => {
        val prevFut = bareDAO.get(
          BareAllChampionStatisticsDAO.Key(
            allChampions,
            tiers,
            Set(patch),
            regions,
            roles,
            queues,
            enemies,
            reverse
          )
        )
        val curFut = bareDAO.get(
          BareAllChampionStatisticsDAO.Key(
            allChampions,
            tiers,
            patches,
            regions,
            roles,
            queues,
            enemies,
            reverse
          )
        )
        (prevFut |@| curFut).map { (prev, cur) =>
          ChangeMarker.mark(cur, prev)
        }
      }
      case None => {
        bareDAO.get(
          BareAllChampionStatisticsDAO.Key(
            allChampions, tiers, patches, regions,
            roles, queues, enemies, reverse
          )
        )
      }
    }
  }

}
