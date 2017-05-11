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

      // Find the champions that satisfy the min play rate for their role
      val pickStats = statistics.sums.flatMap(_.subscalars).map(_.picks).getOrElse(Seq())

      val roleStats = pickStats.filter(x => key.roles(x.role)).map(_.picks).toList.combineAll
      val totalStats = pickStats.map(_.picks).toList.combineAll

      // filter champions with appropriate pick rate for their role
      val champs = roleStats.filter {
        case (champ, rolePlays) => {
          // Get their total plays
          val totalPlays = totalStats.get(champ).orEmpty

          // if they have plays, filter if they have enough percentage in the role
          if (totalPlays > 0) {
            val pickRate = rolePlays / totalPlays
            pickRate >= key.constraints.minChampInRoleRate

          // otherwise don't display
          } else {
            false
          }
        }
      }.keys

      // Filter maps for keys that contain the champion
      if (champs.isEmpty) {
        results
      } else {
        results.filterChampions(champs.toSet)
      }
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
