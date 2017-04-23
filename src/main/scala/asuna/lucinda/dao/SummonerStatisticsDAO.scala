package asuna.lucinda.dao

import asuna.common.legends.MatchSumHelpers._
import asuna.lucinda.matches.StatisticsGenerator
import cats.implicits._
import asuna.proto.league.SummonerId
import asuna.proto.league.alexandria.rpc.GetSummonerMatchSumRequest
import asuna.proto.league.{ Queue, Role, Tier }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.lucinda.Statistics
import monix.eval.Task
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.lucinda.rpc.Constraints

object SummonerStatisticsDAO {
  case class Key(
    id: SummonerId,
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

  def fetchACS(in: SummonerStatisticsDAO.Key, role: Role): Task[AllChampionStatistics] = {
    val base = in.base
    summonerChampionsDAO.get(
      in.id,
      base.allChampions,
      base.prevPatch,
      Set(role),
      base.patches,
      base.queues,
      base.enemies
    )
  }

  def fetchPatchACS(in: SummonerStatisticsDAO.Key, role: Role, patch: String): Task[AllChampionStatistics] = {
    val base = in.base
    summonerChampionsDAO.get(
      in.id,
      base.allChampions,
      None,
      Set(role),
      Set(patch),
      base.queues,
      base.enemies
    )
  }

}
