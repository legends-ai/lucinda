package io.asuna.lucinda

import io.asuna.asunasan.Config
import io.asuna.proto.service_vulgate.{ VulgateGrpc, VulgateRpc }
import io.asuna.proto.vulgate.VulgateData
import scala.concurrent.{ExecutionContext, Future}

import scalaz.Scalaz._
import io.asuna.lucinda.dao.{ChampionDAO, MatchAggregateDAO, ChampionStatisticsDAO}
import io.asuna.lucinda.database.{Connector, LucindaDatabase}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import redis.RedisClient

class LucindaServer(config: Config[LucindaConfig]) extends LucindaGrpc.Lucinda {

  implicit val akkaSystem = akka.actor.ActorSystem()

  // TODO(igm): find an ec that supports rate limiting. We don't want to fuck up Cassandra.
  implicit val ec = ExecutionContext.global

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
    db = 0.some,
    name = "lucinda:stats"
  )
  lazy val aggRedis = RedisClient(
    host = config.service.redisHost,
    port = config.service.redisPort,
    db = 1.some,
    name = "lucinda:aggs"
  )

  lazy val championStatisticsDAO = new ChampionStatisticsDAO(db, statsRedis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(db, aggRedis, championStatisticsDAO)
  lazy val championDAO = new ChampionDAO(vulgate, championStatisticsDAO, matchAggregateDAO)

  override def getStatistics(req: GetStatisticsRequest) = logFuture {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = VulgateHelpers.makeVulgateContext(req.patch, req.region).some,
          patches = req.patch,
          tiers = req.tier
        )
      )
      statistics <- championStatisticsDAO.getWithRoles(factors, req.region, forceRefresh = req.forceRefresh)
    } yield GetStatisticsResponse(
      statistics = statistics
    )
  }

  override def getChampion(req: GetChampionRequest) = logFuture {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = VulgateHelpers.makeVulgateContext(req.patch, req.region).some,
          patches = req.patch,
          tiers = req.tier
        )
      )
      champ <- championDAO.getChampion(factors, req.championId, req.region, req.role, req.minPlayRate, forceRefresh = req.forceRefresh)
    } yield champ
  }

  override def getMatchup(req: GetMatchupRequest) = logFuture {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
          context = VulgateHelpers.makeVulgateContext(req.patch, req.region).some,
          patches = req.patch,
          tiers = req.tier
        )
      )
      matchup <- championDAO.getMatchup(factors, req.focusChampionId, req.region, req.role, req.minPlayRate, req.enemyChampionId, forceRefresh = req.forceRefresh)
    } yield matchup
  }

  override def getMatchSum(req: GetMatchSumRequest) = logFuture {
    db.matchSums.sum(req.filters.toSet)
  }

  private[this] def logFuture[T](fut: => Future[T]): Future[T] = {
    fut.onFailure {
      case t => {
        println("An error has occured: " + t.getMessage)
        t.printStackTrace()
      }
    }
    fut
  }

}
