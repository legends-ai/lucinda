package asuna.lucinda.dao

import asuna.lucinda.matches.MinPickRateDecorator
import asuna.lucinda.matches.StatisticsGenerator
import asuna.proto.league._
import asuna.proto.league.lucinda.Statistics
import monix.eval.Task

class StatisticsDAO(bareDAO: BareStatisticsDAO) {

  def get(
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
    minPickRate: Double
  ): Task[Statistics] = {
    bareDAO.get(
      BareStatisticsDAO.Key(
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
      )
    ) map { stats =>
      MinPickRateDecorator.decorate(minPickRate, 10, stats)
    }
  }

}
