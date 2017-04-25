package asuna.lucinda.dao

import asuna.lucinda.statistics.ChangeMarker
import asuna.proto.league.{ Queue, Role, AccountId, Tier }
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import cats.implicits._
import monix.cats._
import monix.eval.Task

object SummonerChampionsDAO {
  case class Key(
    id: AccountId,
    allChampions: Set[Int],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ) extends SummonerKey with BaseAllChampionStatisticsDAO.CompositeKey {
    val base = BaseAllChampionStatisticsDAO.Key(
      allChampions = allChampions,
      tiers = Set(Tier.UNRANKED),
      patches = patches,
      regions = Set(id.region),
      roles = roles,
      queues = queues,
      enemies = enemies,
      reverse = reverse
    )
  }
}

class SummonerChampionsDAO(alexandria: Alexandria, sf: SumFetcher[SummonerChampionsDAO.Key])
    extends BaseAllChampionStatisticsDAO[SummonerChampionsDAO.Key] {
  import SummonerChampionsDAO.Key

  val sumFetcher = sf

  def getResults(
    id: AccountId,
    allChampions: Set[Int],
    prevPatch: Option[String],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyIds: Set[Int]
  ): Task[AllChampionStatistics.Results] = {
    get(
      id, allChampions, prevPatch, roles, patches, queues, enemyIds
    ).map(_.results.getOrElse(AllChampionStatistics.Results()))
  }

  def get(
    id: AccountId,
    allChampions: Set[Int],
    prevPatch: Option[String],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyIds: Set[Int]
  ): Task[AllChampionStatistics] = {
    prevPatch match {
      case Some(patch) => {
        val prevFut = compute(Key(id, allChampions, roles, Set(patch), queues, enemyIds))
        val curFut = compute(Key(id, allChampions, roles, patches, queues, enemyIds))
        (prevFut |@| curFut).map { (prev, cur) =>
          ChangeMarker.mark(cur, prev)
        }
      }
      case None => compute(Key(id, allChampions, roles, patches, queues, enemyIds))
    }
  }

}
