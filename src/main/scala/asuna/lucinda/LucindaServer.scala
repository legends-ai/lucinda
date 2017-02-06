package asuna.lucinda

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import asuna.common.BaseGrpcService
import asuna.proto.league.{ QueueType, MatchSum }
import asuna.proto.league.alexandria.AlexandriaGrpc
import asuna.proto.league.alexandria.rpc.GetSumRequest
import asuna.proto.league.lucinda.{ AllChampionStatistics, LucindaGrpc, Statistics, SummonerOverview }
import asuna.proto.league.lucinda.rpc._
import asuna.proto.league.vulgate.VulgateGrpc
import asuna.proto.league.vulgate.rpc.GetAggregationFactorsRequest
import asuna.lucinda.dao.{ MatchupDAO, StatisticsDAO, AllChampionStatisticsDAO }
import cats.implicits._
import redis.RedisClient

class LucindaServer(args: Seq[String])
    extends BaseGrpcService(args, LucindaConfigParser, LucindaGrpc.bindService)
    with LucindaGrpc.Lucinda {

  implicit val akkaSystem = akka.actor.ActorSystem()

  val alexandria = AlexandriaGrpc.stub(clientFor("alexandria"))
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

  lazy val allChampionStatisticsDAO = new AllChampionStatisticsDAO(config.service, alexandria, statsRedis)
  lazy val statisticsDAO = new StatisticsDAO(alexandria, aggRedis, allChampionStatisticsDAO)
  lazy val matchupDAO = new MatchupDAO(allChampionStatisticsDAO)

  override def getAllChampions(req: GetAllChampionsRequest): Future[AllChampionStatistics.Results] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patches, req.regions.head)),
          patches = req.patches
        )
      )
      results <- allChampionStatisticsDAO.getResults(
        allChampions = factors.champions.toSet,
        patches = req.patches.toSet,
        prevPatches = factors.prevPatches,
        tiers = req.tiers.toSet,
        regions = req.regions.toSet,
        roles = req.roles.toSet,
        queues = defaultQueuesIfEmpty(req.queues),
        enemies = req.enemyIds.toSet,
        forceRefresh = req.forceRefresh,
        minPlayRate = req.minPlayRate
      )
    } yield results
  }

  override def getStatistics(req: GetStatisticsRequest): Future[Statistics] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patches, req.regions.head)),
          patches = req.patches
        )
      )
      statistics <- statisticsDAO.get(
        allChampions = factors.champions.toSet,
        patches = req.patches.toSet,
        lastFivePatches = factors.lastFivePatches, //
        prevPatches = factors.prevPatches,
        champions = req.championIds.toSet,
        tiers = req.tiers.toSet,
        regions = req.regions.toSet,
        roles = req.roles.toSet,
        queues = defaultQueuesIfEmpty(req.queues),
        enemies = req.enemyIds.toSet,
        minPlayRate = req.minPlayRate,
        forceRefresh = req.forceRefresh
      )
    } yield statistics
  }

  override def getAllMatchups(req: GetAllMatchupsRequest): Future[GetAllMatchupsResponse] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(req.patches, req.region)),
          patches = req.patches
        )
      )

      // Get all of the matchup overviews of this champion
      matchups <- matchupDAO.getMatchupOverviews(
        allChampions = factors.champions.toSet,
        patches = req.patches.toSet,
        prevPatches = factors.prevPatches,
        tiers = req.tiers.toSet,
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


  def getAllSummonerChampions(request: GetAllSummonerChampionsRequest): Future[AllChampionStatistics.Results] = ???

  def getSummonerOverview(request: GetSummonerRequest): Future[SummonerOverview] = ???

  def getSummonerStatistics(request: GetSummonerRequest): Future[Statistics] = ???

}
