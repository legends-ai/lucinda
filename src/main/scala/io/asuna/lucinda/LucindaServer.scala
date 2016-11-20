package io.asuna.lucinda

import io.asuna.asunasan.Config
import io.asuna.proto.service_vulgate.{ VulgateGrpc, VulgateRpc }
import scala.concurrent.{ExecutionContext, Future}

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
  // We'll use lazy vals because the compiler should do the work of figuring the order out.

  lazy val redis = RedisClient() // TOOD(igm): use separate redis databases for all types of data we store
  lazy val championDAO = new ChampionDAO(championStatisticsDAO, matchAggregateDAO)
  lazy val championStatisticsDAO = new ChampionStatisticsDAO(db, redis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(db, redis, championStatisticsDAO)

  override def getStatistics(req: GetStatisticsRequest) = {
    val reply = ChampionStatistics()
    Future.successful(reply)
  }

  override def getChampion(req: GetChampionRequest) = {
    for {
      statisticsCtx <- vulgate.getStatisticsContext(
        VulgateRpc.GetStatisticsContextRequest(
          patches = req.patch,
          tiers = req.tier
        )
      )
      matchAggregate <- matchAggregateDAO.get(
        statisticsCtx.champions.toSet, statisticsCtx.patches.toSet, statisticsCtx.lastFivePatches.toSet,
        req.championId, statisticsCtx.tiers.toSet, req.region, req.role, -1, req.minPlayRate
      )
    } yield {
      Champion(
        matchAggregate = Some(matchAggregate)
      )
    }
  }

  override def getMatchup(req: GetMatchupRequest) = {
    Future.successful(Matchup())
  }

  override def getMatchSum(req: GetMatchSumRequest) = {
    db.matchSums.sum(req.filters.toSet)
  }

}
