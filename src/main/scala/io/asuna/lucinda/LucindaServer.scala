package io.asuna.lucinda

import io.asuna.asunasan.BaseService
import io.asuna.proto.service_vulgate.{ VulgateGrpc, VulgateRpc }
import scala.concurrent.{ExecutionContext, Future}

import io.asuna.lucinda.dao.{ChampionDAO, MatchAggregateDAO, ChampionStatisticsDAO}
import io.asuna.lucinda.database.{Connector, LucindaDatabase}
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import redis.RedisClient

import scala.concurrent.ExecutionContext.Implicits.global

class LucindaServer(args: Seq[String]) extends BaseService(args, LucindaConfigParser) with LucindaGrpc.Lucinda {

  override val serviceDefinition = LucindaGrpc.bindService(this, implicitly[ExecutionContext])

  implicit val akkaSystem = akka.actor.ActorSystem()

  // Setup database
  val connector = Connector.fromConfig(config)
  val db = new LucindaDatabase(connector)

  // Setup vulgate connection
  val vulgateConn = config.asuna.vulgate.conn
  val vulgate = VulgateGrpc.stub(vulgateConn)

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

  override def getStatistics(req: GetStatisticsRequest) = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      statistics <- championStatisticsDAO.get(
        factors = factors,
        region = req.region,
        role = req.role,
        forceRefresh = req.forceRefresh
      )
    } yield statistics
  }

  override def getChampion(req: GetChampionRequest) = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      champ <- championDAO.getChampion(factors, req.championId, req.region, req.role, req.minPlayRate, forceRefresh = req.forceRefresh)
    } yield champ
  }


  override def getMatchup(req: GetMatchupRequest) = endpoint {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      matchup <- championDAO.getMatchup(factors, req.focusChampionId, req.region, req.role, req.minPlayRate, req.enemyChampionId, forceRefresh = req.forceRefresh)
    } yield matchup
  }

  override def getMatchSum(req: GetMatchSumRequest) = endpoint {
    db.matchSums.sum(req.filters.toSet)
  }

}
