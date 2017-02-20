package asuna.lucinda

import asuna.common.AsunaError
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import asuna.common.BaseGrpcService
import asuna.proto.league.{ Queue, MatchSum }
import asuna.proto.league.alexandria.AlexandriaGrpc
import asuna.proto.league.alexandria.rpc.GetSumRequest
import asuna.proto.league.lucinda._
import asuna.proto.league.lucinda.rpc._
import asuna.proto.league.vulgate.VulgateGrpc
import asuna.proto.league.vulgate.rpc.GetAggregationFactorsRequest
import asuna.lucinda.dao._
import cats.implicits._

class LucindaServer(args: Seq[String])
    extends BaseGrpcService(args, LucindaConfigParser, LucindaGrpc.bindService)
    with LucindaGrpc.Lucinda {

  val alexandria = AlexandriaGrpc.stub(clientFor("alexandria"))
  val vulgate = VulgateGrpc.stub(clientFor("vulgate"))

  lazy val allChampionStatisticsDAO = new AllChampionStatisticsDAO(config.service, alexandria)
  lazy val statisticsDAO = new StatisticsDAO(alexandria, allChampionStatisticsDAO)
  lazy val matchupDAO = new MatchupDAO(allChampionStatisticsDAO)

  lazy val summonerChampionsDAO = new SummonerChampionsDAO(alexandria)

  override def getAllChampions(req: GetAllChampionsRequest): Future[AllChampionStatistics.Results] = {
    if (!req.query.isDefined) {
      Future.failed(new AsunaError("query unspecified"))
    }
    val key = req.query.get
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(key.patches, key.regions.head)),
          patches = key.patches
        )
      )
      results <- allChampionStatisticsDAO.getResults(
        allChampions = factors.champions.toSet,
        prevPatches = factors.prevPatches,

        patches = key.patches.toSet,
        tiers = key.tiers.toSet,
        regions = key.regions.toSet,
        roles = key.roles.toSet,
        queues = defaultQueuesIfEmpty(key.queues),
        enemies = key.enemyIds.toSet,

        forceRefresh = req.forceRefresh,
        minPlayRate = req.minPlayRate
      )
    } yield results
  }

  override def getStatistics(req: GetStatisticsRequest): Future[Statistics] = {
    if (!req.query.isDefined) {
      Future.failed(new AsunaError("query unspecified"))
    }
    val key = req.query.get
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = Some(VulgateHelpers.makeVulgateContext(key.patches, key.regions.head)),
          patches = key.patches
        )
      )
      statistics <- statisticsDAO.get(
        allChampions = factors.champions.toSet,
        lastFivePatches = factors.lastFivePatches, //
        prevPatches = factors.prevPatches,

        patches = key.patches.toSet,
        champions = key.championIds.toSet,
        tiers = key.tiers.toSet,
        regions = key.regions.toSet,
        roles = key.roles.toSet,
        queues = defaultQueuesIfEmpty(key.queues),
        enemies = key.enemyIds.toSet,

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

  private[this] def defaultQueuesIfEmpty(queues: Seq[Queue]): Set[Queue] =
    if (queues.length == 0) config.service.defaultQueues else queues.toSet


  def getAllSummonerChampions(req: GetAllSummonerChampionsRequest): Future[AllChampionStatistics.Results] = {
    if (!req.summonerId.isDefined) {
      Future.failed(new AsunaError("summoner id unspecified"))
    }
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = VulgateHelpers.makeVulgateContext(req.patches, req.summonerId.get.region).some,
          patches = req.patches
        )
      )
      results <- summonerChampionsDAO.get(
        id = req.summonerId.get,
        allChampions = factors.champions.toSet,
        prevPatch = factors.prevPatches.get(factors.earliestPatch),

        roles = req.role.toSet,
        patches = req.patches.toSet,
        queues = req.queues.toSet,
        enemyIds = req.enemyIds.toSet
      )
    } yield results
  }

  def getSummonerOverview(req: GetSummonerRequest): Future[SummonerOverview] = ???

  def getSummonerStatistics(req: GetSummonerRequest): Future[Statistics] = ???

}
