package asuna.lucinda.dao

import asuna.lucinda.matches.MinPickRateDecorator
import asuna.proto.league._
import asuna.proto.league.lucinda.Statistics
import asuna.proto.league.lucinda.rpc.Constraints
import monix.eval.Task

object StatisticsDAO {

  case class Key(
    baseKey: BareStatisticsDAO.Key,
    constraints: Constraints
  )

  object Key {

    def apply(
      allChampions: Set[Int],
      patches: Set[String],
      patchNeighborhood: Seq[String],
      prevPatch: Option[String],
      champions: Set[Int],
      tiers: Set[Tier],
      regions: Set[Region],
      roles: Set[Role],
      queues: Set[Queue],
      enemies: Set[Int],
      constraints: Constraints
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
            queues = queues
          )
        ),
        constraints = constraints
      )
    }
  }

}

class StatisticsDAO(bareDAO: BareStatisticsDAO) {

  def compute(key: StatisticsDAO.Key): Task[Statistics] = {
    bareDAO.batchedGet(key.baseKey, forceRefresh = key.constraints.forceRefresh).map { stats =>
      MinPickRateDecorator.decorate(key.constraints.minPickRate, 10, stats)
    }
  }

}
