package asuna.lucinda.dao

import asuna.common.legends.MatchSumHelpers._
import asuna.lucinda.statistics.{ ChangeMarker, StatisticsAggregator }
import asuna.proto.league.{ SummonerMatchSum, Tier }
import asuna.proto.league.alexandria.rpc.GetSummonerMatchSumRequest
import asuna.proto.league.{ MatchFiltersSpace, Queue, Role, SummonerId }
import asuna.proto.league.lucinda.AllChampionStatistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import scala.concurrent.{ ExecutionContext, Future }
import cats.implicits._


class SummonerChampionsDAO(alexandria: Alexandria)(implicit ec: ExecutionContext) {

  def getResults(
    id: SummonerId,
    allChampions: Set[Int],
    prevPatch: Option[String],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyIds: Set[Int]
  ): Future[AllChampionStatistics.Results] = {
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
  ): Future[AllChampionStatistics] = {
    prevPatch match {
      case Some(patch) => {
        val prevFut = bareGet(id, allChampions, roles, Set(patch), queues, enemyIds)
        val curFut = bareGet(id, allChampions, roles, patches, queues, enemyIds)
        (prevFut |@| curFut).map { (prev, cur) =>
          ChangeMarker.mark(cur, prev)
        }
      }
      case None => bareGet(id, allChampions, roles, patches, queues, enemyIds)
    }
  }

  private def bareGet(
    id: SummonerId,
    allChampions: Set[Int],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyIds: Set[Int]
  ): Future[AllChampionStatistics] = {
    val space = MatchFiltersSpace(
      roles = roles.toSeq,
      regions = Seq(id.region),
      versions = patches.toSeq,
      queues = queues.toSeq,
      tiers = Seq(Tier.UNRANKED),
      enemyIds = enemyIds.toSeq
    )

    // TODO(igm): abstract along with AllChampionStatisticsDAO
    val champQueriesMap = allChampions
      .map(x => (x, x)).toMap
      .mapValues { champ =>
        space.copy(championIds = Seq(champ))
      }

    val futs: Map[Int, Future[SummonerMatchSum]] = champQueriesMap.mapValues { subspace =>
      val req = GetSummonerMatchSumRequest(
        summoner = id.some,
        space = subspace.some
      )
      alexandria.getSummonerMatchSum(req)
    }

    futs.sequence.map { sumsMap =>
      StatisticsAggregator.makeStatistics(sumsMap.mapValues(_.matchSum.orEmpty))
    }
  }

}
