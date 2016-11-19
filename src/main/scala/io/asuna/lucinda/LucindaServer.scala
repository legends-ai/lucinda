package io.asuna.lucinda

import io.asuna.asunasan.Config
import scala.concurrent.{ExecutionContext, Future}

import io.asuna.lucinda.dao.{MatchAggregateDAO, ChampionStatisticsDAO}
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

  val vulgateConn = config.asuna.vulgate

  // Next, let's init all of our dependencies.
  // We'll use lazy vals because the compiler should do the work of figuring the order out.
  lazy val redis = RedisClient()
  lazy val championStatisticsDAO = new ChampionStatisticsDAO(db, redis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(db, redis, championStatisticsDAO)

  override def getStatistics(req: GetStatisticsRequest) = {
    val reply = ChampionStatistics()
    Future.successful(reply)
  }

  override def getChampion(req: GetChampionRequest) = {
    Future.successful(Champion())
  }

  override def getMatchup(req: GetMatchupRequest) = {
    Future.successful(Matchup())
  }

  override def getMatchSum(req: GetMatchSumRequest) = {
    db.matchSums.sum(req.filters.toSet)
  }

}
