package asuna.lucinda.dao

import asuna.lucinda.statistics.ChangeMarker
import asuna.proto.league.lucinda.rpc.Constraints
import monix.eval.Task
import asuna.lucinda.statistics.FilterChampionsHelpers._
import asuna.proto.league._
import asuna.proto.league.lucinda.AllChampionStatistics
import cats.implicits._
import monix.cats._

object AllChampionStatisticsDAO {

  case class Key(
    allChampions: Set[Int],
    tiers: Set[Tier],
    prevPatch: Option[String],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false,

    constraints: Constraints = Constraints()
  ) {

    lazy val bareKey: BareAllChampionStatisticsDAO.Key = {
      BareAllChampionStatisticsDAO.Key(
        BaseAllChampionStatisticsDAO.Key(
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
    }

    def patchBareKey(patch: String): BareAllChampionStatisticsDAO.Key = {
      bareKey.copy(bareKey.base.copy(patches = Set(patch)))
    }

  }

}

class AllChampionStatisticsDAO(bareDAO: BareAllChampionStatisticsDAO)
  extends EphemeralDAO[AllChampionStatisticsDAO.Key, AllChampionStatistics] {

  import AllChampionStatisticsDAO.Key

  /**
   * Fetches a AllChampionStatistics.Results object.
   */
  def getResults(key: Key): Task[AllChampionStatistics.Results] = {
    compute(key).map { statistics =>
      // Get the results object
      val results = statistics.results.getOrElse(AllChampionStatistics.Results())

      // Find the champions that satisfy the min play rate
      val pickRates = results.derivatives
        .map(_.picks.mapValues(_.mean)).orEmpty
      val champs = pickRates.filter {
        case (champ, pickRate) => pickRate >= key.constraints.minPickRate
      }.keys

      // Filter maps for keys that contain the champion
      results.filterChampions(champs.toSet)
    }
  }

  /**
    * Gets a AllChampionStatistics object with caching.
    */
  def compute(key: Key): Task[AllChampionStatistics] = {
    val curTask = bareDAO.get(
      key.bareKey,
      forceRefresh = key.constraints.forceRefresh
    )
    key.prevPatch match {
      case Some(patch) => {
        val prevTask = bareDAO.get(
          key.patchBareKey(patch),
          forceRefresh = key.constraints.forceRefresh
        )
        (prevTask |@| curTask).map { (prev, cur) =>
          ChangeMarker.mark(cur, prev)
        }
      }
      case None => curTask
    }
  }


}
