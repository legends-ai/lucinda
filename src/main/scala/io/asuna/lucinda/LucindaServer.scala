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

  lazy val championStatisticsDAO = new ChampionStatisticsDAO(vulgate, db, statsRedis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(db, aggRedis, championStatisticsDAO)
  lazy val championDAO = new ChampionDAO(vulgate, championStatisticsDAO, matchAggregateDAO)

  override def getStatistics(req: GetStatisticsRequest) = {
    for {
      statistics <- championStatisticsDAO.getWithRoles(req.tier, req.patch, req.region)
    } yield GetStatisticsResponse(
      statistics = statistics
    )
  }

  override def getChampion(req: GetChampionRequest) = {
    championDAO.getChampion(req.tier, req.patch, req.championId, req.region, req.role, req.minPlayRate)
  }

  override def getMatchup(req: GetMatchupRequest) = {
    championDAO.getMatchup(req.tier, req.patch, req.focusChampionId, req.region, req.role, req.minPlayRate, req.enemyChampionId)
  }

  override def getMatchSum(req: GetMatchSumRequest) = {
    db.matchSums.sum(req.filters.toSet)
  }

}
