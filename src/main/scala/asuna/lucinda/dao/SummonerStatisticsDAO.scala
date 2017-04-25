package asuna.lucinda.dao

import asuna.proto.league.AccountId
import asuna.proto.league.{ Queue, Role, Tier }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.lucinda.rpc.Constraints
import monix.eval.Task

object SummonerStatisticsDAO {
  case class Key(
    id: AccountId,
    allChampions: Set[Int],
    patches: Set[String],
    patchNeighborhood: Seq[String],
    prevPatch: Option[String],

    champions: Set[Int],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue],
  ) extends SummonerKey with BaseStatisticsDAO.CompositeKey {
    val base = BaseStatisticsDAO.Key(
      allChampions = allChampions,
      patches = patches,
      patchNeighborhood = patchNeighborhood,
      prevPatch = prevPatch,

      champions = champions,
      tiers = Set(Tier.UNRANKED),
      regions = Set(id.region),
      roles = roles,
      enemies = enemies,
      queues = queues,
      constraints = Constraints(),
    )
  }
}

/**
  * TODO(igm): merge with BareStatisticsDAO
  */
class SummonerStatisticsDAO(
  alexandria: Alexandria,
  summonerChampionsDAO: SummonerChampionsDAO,
  sf: SumFetcher[SummonerStatisticsDAO.Key]
) extends BaseStatisticsDAO[SummonerStatisticsDAO.Key] {

  val sumFetcher = sf

  def fetchACS(in: SummonerStatisticsDAO.Key, roles: Set[Role]): Task[AllChampionStatistics] = {
    val base = in.base
    summonerChampionsDAO.get(
      in.id,
      base.allChampions,
      base.prevPatch,
      roles,
      base.patches,
      base.queues,
      base.enemies
    )
  }

  def fetchPatchACS(in: SummonerStatisticsDAO.Key, roles: Set[Role], patch: String): Task[AllChampionStatistics] = {
    val base = in.base
    summonerChampionsDAO.get(
      in.id,
      base.allChampions,
      None,
      roles,
      Set(patch),
      base.queues,
      base.enemies
    )
  }

}
