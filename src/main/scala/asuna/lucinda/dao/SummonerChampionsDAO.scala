package asuna.lucinda.dao

import asuna.common.legends.MatchSumHelpers._
import asuna.lucinda.statistics.{ ChangeMarker, StatisticsAggregator }
import asuna.proto.league.{ SummonerMatchSum, Tier }
import asuna.proto.league.alexandria.rpc.GetSummonerMatchSumRequest
import asuna.proto.league.{ MatchFiltersSpace, Queue, Role, SummonerId }
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import monix.eval.Task
import scala.concurrent.{ ExecutionContext, Future }
import cats.implicits._
import monix.cats._

object SummonerChampionsDAO {
  case class Key(
    id: SummonerId,
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
    id: SummonerId,
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
    id: SummonerId,
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
