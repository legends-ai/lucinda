package asuna.lucinda

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import asuna.common.BaseGrpcService
import asuna.proto.league.{ ChampionId, QueueType, MatchSum }
import asuna.proto.league.alexandria.AlexandriaGrpc
import asuna.proto.league.alexandria.rpc.GetSumRequest
import asuna.proto.league.lucinda.{ Champion, ChampionStatistics, LucindaGrpc, Matchup }
import asuna.proto.league.lucinda.rpc._
import asuna.proto.league.vulgate.VulgateGrpc
import asuna.proto.league.vulgate.rpc.GetAggregationFactorsRequest
import asuna.lucinda.dao.{ ChampionDAO, MatchAggregateDAO, ChampionStatisticsDAO }
import cats.implicits._
import redis.RedisClient


class LucindaServer(args: Seq[String])
    extends BaseGrpcService(args, LucindaConfigParser, LucindaGrpc.bindService)
    with LucindaGrpc.Lucinda {

  implicit val akkaSystem = akka.actor.ActorSystem()

  // Setup alexandria connection
  val alexandria = AlexandriaGrpc.stub(clientFor("alexandria"))

  // Setup vulgate connection
  val vulgate = VulgateGrpc.stub(clientFor("vulgate"))

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

  lazy val championStatisticsDAO = new ChampionStatisticsDAO(config.service, alexandria, statsRedis)
  lazy val matchAggregateDAO = new MatchAggregateDAO(alexandria, aggRedis, championStatisticsDAO)
  lazy val championDAO = new ChampionDAO(vulgate, championStatisticsDAO, matchAggregateDAO)

  override def getStatistics(req: GetStatisticsRequest): Future[ChampionStatistics.Results] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      results <- championStatisticsDAO.getResults(
        factors = factors,
        region = req.region,
        role = req.role,
        queues = defaultQueuesIfEmpty(req.queues),
        forceRefresh = req.forceRefresh,
        minPlayRate = req.minPlayRate
      )
    } yield results
  }

  override def getChampion(req: GetChampionRequest): Future[Champion] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      champ <- championDAO.getChampion(
        factors = factors,
        champion = req.championId,
        region = req.region,
        role = req.role,
        queues = defaultQueuesIfEmpty(req.queues),
        enemy = None,
        minPlayRate = req.minPlayRate,
        forceRefresh = req.forceRefresh
      )
    } yield champ
  }


  override def getMatchup(req: GetMatchupRequest): Future[Matchup] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )
      matchup <- championDAO.getMatchup(
        factors = factors,
        focus = req.focusChampionId,
        region = req.region,
        role = req.role,
        queues = defaultQueuesIfEmpty(req.queues),
        minPlayRate = req.minPlayRate,
        enemy = req.enemyChampionId,
        forceRefresh = req.forceRefresh
      )
    } yield matchup
  }

  override def getMatchSum(req: GetMatchSumRequest): Future[MatchSum] = {
    alexandria.getSum(GetSumRequest(req.filters.toSet.toSeq))
  }

  override def getAllMatchups(req: GetAllMatchupsRequest): Future[GetAllMatchupsResponse] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patch, req.region)),
          patches = req.patch,
          tiers = req.tier
        )
      )

      // Get all of the matchup overviews of this champion
      matchups <- championDAO.getMatchupOverviews(
        factors = factors,
        champion = req.championId,
        region = req.region,
        role = req.role,
        queues = defaultQueuesIfEmpty(req.queues),
        minPlayRate = req.minPlayRate,
        forceRefresh = req.forceRefresh
      )
    } yield GetAllMatchupsResponse(matchups = matchups)
  }

  private[this] def defaultQueuesIfEmpty(queues: Seq[QueueType]): Set[QueueType] =
    if (queues.length == 0) config.service.defaultQueues else queues.toSet

}
