package asuna.lucinda

import asuna.common.AsunaError
import asuna.proto.league.lucinda.rpc.Constraints
import scala.concurrent.{ ExecutionContext, Future }

import monix.execution.Scheduler
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

class LucindaServer(args: Seq[String])(implicit scheduler: Scheduler)
    extends BaseGrpcService(args, LucindaConfigParser, LucindaGrpc.bindService)
    with LucindaGrpc.Lucinda {

  val alexandria = AlexandriaGrpc.stub(clientFor("alexandria"))
  val vulgate = VulgateGrpc.stub(clientFor("vulgate"))

  val summonerSumFetcher = new SummonerSumFetcher(alexandria)
  val allSumFetcher = new AllSumFetcher(alexandria)

  lazy val bareAllChampionStatisticsDAO = new BareAllChampionStatisticsDAO(
    config.service.allChampionStatisticsDAOSettings,
    alexandria,
    statsd,
    allSumFetcher
  )
  lazy val allChampionStatisticsDAO = new AllChampionStatisticsDAO(bareAllChampionStatisticsDAO)
  bareAllChampionStatisticsDAO.startRefreshing

  lazy val bareStatisticsDAO = new BareStatisticsDAO(
    config.service.statisticsDAOSettings, alexandria,
    statsd, allChampionStatisticsDAO, allSumFetcher
  )
  lazy val statisticsDAO = new StatisticsDAO(bareStatisticsDAO)
  bareStatisticsDAO.startRefreshing

  lazy val matchupDAO = new MatchupDAO(allChampionStatisticsDAO)

  lazy val summonerChampionsDAO = new SummonerChampionsDAO(alexandria, summonerSumFetcher)
  lazy val summonerStatisticsDAO = new SummonerStatisticsDAO(alexandria, summonerChampionsDAO, summonerSumFetcher)

  lazy val summonerOverviewDAO = new SummonerOverviewDAO(summonerChampionsDAO)

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
        AllChampionStatisticsDAO.Key(
          allChampions = factors.champions.toSet,
          prevPatch = factors.prevPatches.get(factors.earliestPatch),
          patches = key.patches.toSet,
          tiers = key.tiers.toSet,
          regions = key.regions.toSet,
          roles = key.roles.toSet,
          queues = defaultQueuesIfEmpty(key.queues),
          enemies = key.enemyIds.toSet,
          reverse = false,
          constraints = req.constraints.getOrElse(Constraints())
        )
      ).runAsync
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
          context = VulgateHelpers.makeVulgateContext(key.patches, key.regions.head).some,
          patches = key.patches
        )
      )
      statistics <- statisticsDAO.compute(
        StatisticsDAO.Key(
          allChampions = factors.champions.toSet,
          patchNeighborhood = factors.patchNeighborhood,
          prevPatch = factors.prevPatches.get(factors.earliestPatch),

          patches = key.patches.toSet,
          champions = key.championIds.toSet,
          tiers = key.tiers.toSet,
          regions = key.regions.toSet,
          roles = key.roles.toSet,
          queues = defaultQueuesIfEmpty(key.queues),
          enemies = key.enemyIds.toSet,

          constraints = req.constraints.get
        )
      ).runAsync
    } yield statistics
  }

  override def getAllMatchups(req: GetAllMatchupsRequest): Future[GetAllMatchupsResponse] = {
    for {
      factors <- vulgate.getAggregationFactors(
        GetAggregationFactorsRequest(
          context = VulgateHelpers.makeVulgateContext(req.patches, req.regions.head).some,
          patches = req.patches
        )
      )

      // Get all of the matchup overviews of this champion
      matchups <- matchupDAO.getMatchupOverviews(
        allChampions = factors.champions.toSet,
        patches = req.patches.toSet,
        prevPatch = factors.prevPatches.get(factors.earliestPatch),

        champion = req.championId,
        tiers = req.tiers.toSet,
        regions = req.regions.toSet,
        roles = req.roles.toSet,
        queues = defaultQueuesIfEmpty(req.queues),
        minPickRate = req.constraints.map(_.minPickRate).orEmpty
      ).runAsync
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
      results <- summonerChampionsDAO.getResults(
        id = req.summonerId.get,
        allChampions = factors.champions.toSet,
        prevPatch = factors.prevPatches.get(factors.earliestPatch),

        roles = req.roles.toSet,
        patches = req.patches.toSet,
        queues = req.queues.toSet,
        enemyIds = req.enemyIds.toSet
      ).runAsync
    } yield results
  }

  def getSummonerOverview(req: GetSummonerRequest): Future[SummonerOverview] = {
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
      results <- summonerOverviewDAO.compute(
        SummonerOverviewDAO.Key(
          id = req.summonerId.get,
          allChampions = factors.champions.toSet,
          prevPatch = factors.prevPatches.get(factors.earliestPatch),

          roles = req.roles.toSet,
          patches = req.patches.toSet,
          queues = req.queues.toSet,
          enemyChampionIds = req.enemyChampionIds.toSet
        )
      ).runAsync
    } yield results
  }

  def getSummonerStatistics(req: GetSummonerRequest): Future[Statistics] = {
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
      results <- summonerStatisticsDAO.compute(
        SummonerStatisticsDAO.Key(
          id = req.summonerId.get,
          allChampions = factors.champions.toSet,
          patches = req.patches.toSet,
          patchNeighborhood = factors.patchNeighborhood,
          prevPatch = factors.prevPatches.get(factors.earliestPatch),

          champions = req.championIds.toSet,
          roles = req.roles.toSet,
          queues = req.queues.toSet,
          enemies = req.enemyChampionIds.toSet
        )
      ).runAsync
    } yield results
  }

}
