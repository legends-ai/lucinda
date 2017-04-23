package asuna.lucinda.dao

import asuna.lucinda.matches.MinPickRateDecorator
import asuna.proto.league.lucinda.MatchQuotient.Collections.ItemList
import asuna.proto.league._
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.lucinda.rpc.Constraints
import monix.eval.Task

object StatisticsDAO {

  case class Key(
    baseKey: BareStatisticsDAO.Key,
    boots: Set[Int],
    constraints: Constraints,
  )

  object Key {

    def apply(
      allChampions: Set[Int],
      boots: Set[Int],

      patches: Set[String],
      patchNeighborhood: Seq[String],
      prevPatch: Option[String],
      champions: Set[Int],
      tiers: Set[Tier],
      regions: Set[Region],
      roles: Set[Role],
      queues: Set[Queue],
      enemies: Set[Int],
      constraints: Constraints,
    ): Key = {
      Key(
        baseKey = BareStatisticsDAO.Key(
          base = BaseStatisticsDAO.Key(
            allChampions = allChampions,
            patches = patches,
            patchNeighborhood = patchNeighborhood,
            prevPatch = prevPatch,

            champions = champions,
            tiers = tiers,
            regions = regions,
            roles = roles,
            enemies = enemies,
            queues = queues,

            constraints = constraints,
          )
        ),
        boots = boots,
        constraints = constraints,
      )
    }
  }

  /**
    * An attempt to bring diversity to the suggested build paths.
    * Only one build -- highest picked -- will be returned per 3 item set.
    * This will not take boots into account.
    */
  def diversifyBuilds(in: Seq[ItemList], boots: Set[Int]): Seq[ItemList] = {
    in
      .groupBy(_.items.filterNot(boots).take(3))
      .values.toSeq
      .map(
        _.sortBy(_.subscalars.map(_.playCount).getOrElse(0)).reverse.headOption
      ).flatten
  }

  def diversifyBuildsOfStats(boots: Set[Int])(in: Statistics): Statistics = {
    in.update(_.collections.coreBuilds
              := diversifyBuilds(in.collections.map(_.coreBuilds).getOrElse(Seq()), boots))
  }

}

class StatisticsDAO(bareDAO: BareStatisticsDAO) {
  import StatisticsDAO._

  def compute(key: Key): Task[Statistics] = {
    bareDAO.get(key.baseKey, forceRefresh = key.constraints.forceRefresh)
      .map(diversifyBuildsOfStats(key.boots))
      .map { stats =>
        MinPickRateDecorator.decorate(key.constraints.minPickRate, 10, stats)
      }
  }

}
