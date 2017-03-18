package asuna.lucinda.dao

import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.lucinda.statistics.StatisticsAggregator
import asuna.proto.league._
import asuna.proto.league.lucinda.AllChampionStatistics
import monix.eval.Task
import cats.implicits._
import monix.cats._

object BaseAllChampionStatisticsDAO {

  case class Key(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ) {

    /**
      * Map of every champion to the filters space.
      */
    lazy val filtersMap: Map[Int, MatchFiltersSpace] = allChampions
      .map(c => (c, c)).toMap
      .mapValues { champ =>
        val basis = MatchFilterSpaceHelpers.generate(
          Set(champ), patches, tiers, regions, enemies, roles, queues
        )
        if (reverse) {
          basis.copy(championIds = basis.enemyIds, enemyIds = basis.championIds)
        } else {
          basis
        }
      }

  }

  trait CompositeKey {
    def base: Key
  }

}

trait BaseAllChampionStatisticsDAO[K <: BaseAllChampionStatisticsDAO.CompositeKey] extends EphemeralDAO[K, AllChampionStatistics] {

  implicit val sumFetcher: SumFetcher[K]

  def compute(in: K): Task[AllChampionStatistics] = {
    for {
      // Here, we'll compute the MatchSums. This is where the function is no longer
      // pure, and we make a database call. (Note that since we're using futures,
      // no database call is made at the time of execution.) This returns a
      // Map[Int, Task[MatchSum]].
      sumsMap <- in.base.filtersMap.traverse { space =>
        sumFetcher.fetchSums(in, space)
      }

      // Finally, we'll map over the values of this map to generate a Statistics
      // object for each value. Thus we end up with a Future[AllChampionStatistics],
      // and we are done.
    } yield StatisticsAggregator.makeStatistics(sumsMap)
  }

}
