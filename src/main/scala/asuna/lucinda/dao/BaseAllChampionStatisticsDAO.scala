package asuna.lucinda.dao

import asuna.common.monix.TaskHelpers._
import asuna.lucinda.statistics.StatisticsAggregator
import asuna.proto.league._
import asuna.proto.league.lucinda.AllChampionStatistics
import monix.eval.Task
import cats.implicits._

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

    lazy val protoBasis: MatchFiltersSpace = MatchFiltersSpace(
      enemyIds = enemies.toSeq,
      versions = patches.toSeq,
      tiers = tiers.toSeq,
      regions = regions.toSeq,
      roles = roles.toSeq,
      queues = queues.toSeq,
    )

    /**
      * Map of every champion to the filters space.
      */
    lazy val filtersMap: Map[Int, MatchFiltersSpace] = allChampions
      .map(c => (c, c)).toMap
      .mapValues { champ =>
        val basis = protoBasis.withChampionIds(Set(champ).toSeq)
        if (reverse) {
          basis.copy(championIds = basis.enemyIds, enemyIds = basis.championIds)
        } else {
          basis
        }
      }

    lazy val bansSpace: MatchFiltersSpace =
      protoBasis
        .withChampionIds(allChampions.toSeq)
        // get all data across roles and enemies to fetch everything for this filter combination.
        // everything else is a valid partition to compute ban rate for.
        .clearRoles.clearEnemyIds
  }

  trait CompositeKey {
    def base: Key
  }

}

trait BaseAllChampionStatisticsDAO[K <: BaseAllChampionStatisticsDAO.CompositeKey] extends EphemeralDAO[K, AllChampionStatistics] {
  import asuna.common.legends.MatchSumSeqElement._

  implicit val sumFetcher: SumFetcher[K]

  def compute(in: K): Task[AllChampionStatistics] = {
    for {
      // Here, we'll compute the MatchSums. This is where the function is no longer
      // pure, and we make a database call. (Note that since we're using futures,
      // no database call is made at the time of execution.) This returns a
      // Map[Int, Task[MatchSum]].
      sumsMap <- in.base.filtersMap.traverseG { space =>
        sumFetcher.fetchSums(in, space)
      }

      // Next, we'll compute ban rates by fetching a sum representing the entire space.
      bansSum <- sumFetcher.fetchSums(in, in.base.bansSpace)
      bans = bansSum.collections.map(_.bans.mapValues(_.plays)).getOrElse(Map())

      // We'll also compute picks here.
      picks = (bansSum.collections.map(_.allies) |+| bansSum.collections.map(_.enemies))
        .getOrElse(Seq()).map { elt =>
          AllChampionStatistics.Sums.Subscalars.PickStats(
            role = elt.role,
            picks = elt.stats.mapValues(_.plays),
          )
        }

      // Finally, we'll map over the values of this map to generate a Statistics
      // object for each value. Thus we end up with a Future[AllChampionStatistics],
    } yield StatisticsAggregator.makeStatistics(sumsMap, bans, picks)
  }

}
