package asuna.lucinda.dao

import asuna.lucinda.DAOSettings
import asuna.proto.league._
import asuna.proto.league.alexandria.StoredStatistics
import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistics, StatisticsKey }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc._
import com.google.protobuf.timestamp.Timestamp
import cats.implicits._
import com.timgroup.statsd.StatsDClient
import monix.cats._
import monix.eval.Task


object BareStatisticsDAO {

  /**
    * @param patchNeighborhood Surrounding patches to display stats about.
    */
  case class Key(base: BaseStatisticsDAO.Key)
      extends AllKey with BaseStatisticsDAO.CompositeKey {

    lazy val alexandriaKey: StatisticsKey = StatisticsKey(
      championIds = base.champions.toSeq,
      patches = base.patches.toSeq,
      tiers = base.tiers.toSeq,
      regions = base.regions.toSeq,
      roles = base.roles.toSeq,
      enemyIds = base.enemies.toSeq,
      queues = base.queues.toSeq
    )
  }

}

/**
  * Fetcher for the bare statistics.
  */
class BareStatisticsDAO(
  settings: DAOSettings,
  alexandria: Alexandria,
  statsd: StatsDClient,
  allChampionStatisticsDAO: AllChampionStatisticsDAO,
  sf: SumFetcher[BareStatisticsDAO.Key]
) extends RefreshableProtoDAO[
  BareStatisticsDAO.Key,
  StoredStatistics,
  Statistics
](settings)(statsd) with BaseStatisticsDAO[BareStatisticsDAO.Key] {
  import BareStatisticsDAO.Key

  val sumFetcher = sf

  override def creationTs(stored: StoredStatistics): Option[Timestamp] =
    stored.timestamp

  def fetch(in: Key): Task[Option[StoredStatistics]] = {
    Task.fromFuture {
      alexandria.getStatistics(in.alexandriaKey)
    // only Some if value exists
    }.map(v => v.value.map(_ => v))
  }

  def persist(in: Key, data: Statistics): Task[Unit] = {
    // Insert back to alexandria
    val req = UpsertStatisticsRequest(
      key = in.alexandriaKey.some,
      value = data.some
    )
    Task.deferFuture {
      alexandria.upsertStatistics(req)
    }.map(_ => ())
  }

  def project(full: StoredStatistics): Statistics =
    full.value.get

  def fetchACS(in: BareStatisticsDAO.Key, roles: Set[Role]): Task[AllChampionStatistics] = {
    val base = in.base
    allChampionStatisticsDAO.compute(
      AllChampionStatisticsDAO.Key(
        allChampions = base.allChampions,
        tiers = base.tiers,
        patches = base.patches,
        prevPatch = base.prevPatch,
        regions = base.regions,
        roles = roles,
        queues = base.queues,
        enemies = base.enemies,
        constraints = base.constraints,
      )
    )
  }

  def fetchPatchACS(in: BareStatisticsDAO.Key, roles: Set[Role], patch: String): Task[AllChampionStatistics] = {
    val base = in.base
    allChampionStatisticsDAO.compute(
      AllChampionStatisticsDAO.Key(
        allChampions = base.allChampions,
        tiers = base.tiers,
        patches = Set(patch),
        prevPatch = None,
        regions = base.regions,
        roles = roles,
        queues = base.queues,
        enemies = base.enemies,
        constraints = base.constraints,
      )
    )
  }

}
