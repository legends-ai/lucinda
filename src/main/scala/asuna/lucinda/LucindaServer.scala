package asuna.lucinda

import cats.implicits._
import asuna.common.BaseGrpcService
import asuna.proto.enums.QueueType
import asuna.proto.ids.ChampionId
import asuna.proto.lucinda.LucindaData.{ Champion, ChampionStatistics, Matchup }
import asuna.proto.match_sum.MatchSum
import asuna.proto.service_alexandria.AlexandriaGrpc
import asuna.proto.service_alexandria.AlexandriaRpc.GetSumRequest
import asuna.proto.service_vulgate.{ VulgateGrpc, VulgateRpc }
import scala.concurrent.{ExecutionContext, Future}

import asuna.lucinda.dao.{ChampionDAO, MatchAggregateDAO, ChampionStatisticsDAO}
import asuna.proto.service_lucinda.LucindaGrpc
import asuna.proto.service_lucinda.LucindaRpc._
import redis.RedisClient

import scala.concurrent.ExecutionContext.Implicits.global

class LucindaServer(args: Seq[String])
    extends BaseGrpcService(args, LucindaConfigParser, LucindaGrpc.bindService) with LucindaGrpc.Lucinda {

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
        queues = defaultQueuesIfEmpty(req.queues),
        forceRefresh = req.forceRefresh,
        minPlayRate = req.minPlayRate
      )
    } yield results
  }

  override def getChampion(req: GetChampionRequest): Future[Champion] = {
    for {
      factors <- vulgate.getAggregationFactors(
        VulgateRpc.GetAggregationFactorsRequest(
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
        VulgateRpc.GetAggregationFactorsRequest(
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
        VulgateRpc.GetAggregationFactorsRequest(
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
