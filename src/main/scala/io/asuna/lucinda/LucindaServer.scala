package io.asuna.lucinda

import io.asuna.asunasan.BaseService
import io.asuna.proto.enums.QueueType
import io.asuna.proto.lucinda.LucindaData.{ Champion, ChampionStatistics, Matchup }
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.service_vulgate.{ VulgateGrpc, VulgateRpc }
import scala.concurrent.{ExecutionContext, Future}

import io.asuna.lucinda.dao.{ChampionDAO, MatchAggregateDAO, ChampionStatisticsDAO}
import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import redis.RedisClient

import scala.concurrent.ExecutionContext.Implicits.global

class LucindaServer(args: Seq[String])
    extends BaseService(args, LucindaConfigParser, LucindaGrpc.bindService) with LucindaGrpc.Lucinda {

  implicit val akkaSystem = akka.actor.ActorSystem()

  // Setup database
  val db = LucindaDatabase.fromConfig(config)

  // Setup vulgate connection
  val vulgateConn = config.asuna.services("vulgate").conn
  val vulgate = VulgateGrpc.stub(vulgateConn)

  // Default queues lucinda will serve. TODO(igm): support queue in the requests
  val defaultQueues = Set(QueueType.RANKED_FLEX_SR, QueueType.TEAM_BUILDER_DRAFT_RANKED_5x5)

  // Next, let's init all of our dependencies.
  lazy val statsRedis = RedisClient(
    host = config.service.redisHost,
    port = config.service.redisPort,
    db = Some(0),
    name = "lucinda:stats"
  )
  lazy val aggRedis = RedisClient(
    host = config.service.redisHost,
    port = config.service.redisPort,
    db = Some(1),
    name = "lucinda:aggs"
  )

  lazy val championStatisticsDAO = new ChampionStatisticsDAO(db, statsRedis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(db, aggRedis, championStatisticsDAO)
  lazy val championDAO = new ChampionDAO(vulgate, championStatisticsDAO, matchAggregateDAO)

  override def getStatistics(req: GetStatisticsRequest): Future[ChampionStatistics.Results] = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      results <- championStatisticsDAO.getResults(
        factors = factors,
        region = req.region,
        role = req.role,
        queues = if (req.queues.length == 0) defaultQueues else req.queues.toSet,
        forceRefresh = req.forceRefresh,
        minPlayRate = req.minPlayRate
      )
    } yield results
  }

  override def getChampion(req: GetChampionRequest): Future[Champion] = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      champ <- championDAO.getChampion(
        factors, req.championId, req.region, req.role,
        queues = if (req.queues.length == 0) defaultQueues else req.queues.toSet,
        req.minPlayRate, forceRefresh = req.forceRefresh)
    } yield champ
  }


  override def getMatchup(req: GetMatchupRequest): Future[Matchup] = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      matchup <- championDAO.getMatchup(
        factors, req.focusChampionId, req.region,
        req.role,
        queues = if (req.queues.length == 0) defaultQueues else req.queues.toSet,
        req.minPlayRate, req.enemyChampionId, forceRefresh = req.forceRefresh)
    } yield matchup
  }

  override def getMatchSum(req: GetMatchSumRequest): Future[MatchSum] = endpoint {
    db.matchSums.sum(req.filters.toSet)
  }

}
