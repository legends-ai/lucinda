package asuna.lucinda.dao

import asuna.lucinda.DAOSettings
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.lucinda.statistics.StatisticsAggregator
import asuna.proto.league._
import asuna.proto.league.alexandria.StoredAllChampionStatistics
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc._
import asuna.proto.league.lucinda.{ AllChampionStatistics, AllChampionStatisticsKey }
import com.google.protobuf.timestamp.Timestamp
import cats.implicits._
import com.timgroup.statsd.StatsDClient
import monix.cats._
import monix.eval.Task


object BareAllChampionStatisticsDAO {

  case class Key(base: BaseAllChampionStatisticsDAO.Key)
      extends BaseAllChampionStatisticsDAO.CompositeKey with AllKey {

    lazy val alexandriaKey: AllChampionStatisticsKey = AllChampionStatisticsKey(
      tiers = base.tiers.toSeq,
      patches = base.patches.toSeq,
      regions = base.regions.toSeq,
      roles = base.roles.toSeq,
      enemyIds = base.enemies.toSeq,
      queues = base.queues.toSeq,
      reverse = base.reverse
    )

  }

}

/**
  * Fetcher for the bare statistics.
  */
class BareAllChampionStatisticsDAO(
  settings: DAOSettings,
  alexandria: Alexandria,
  statsd: StatsDClient,
  sf: SumFetcher[BareAllChampionStatisticsDAO.Key]
) extends RefreshableProtoDAO[
  BareAllChampionStatisticsDAO.Key,
  StoredAllChampionStatistics,
  AllChampionStatistics
](settings)(statsd) with BaseAllChampionStatisticsDAO[BareAllChampionStatisticsDAO.Key] {
  import BareAllChampionStatisticsDAO.Key

  val sumFetcher = sf

  override def creationTs(stored: StoredAllChampionStatistics): Option[Timestamp] =
    stored.timestamp

  def fetch(in: Key): Task[Option[StoredAllChampionStatistics]] = {
    Task.fromFuture {
      alexandria.getAllChampionStatistics(in.alexandriaKey)
    // only Some if value exists
    }.map(v => v.value.map(_ => v))
  }

  def persist(in: Key, data: AllChampionStatistics): Task[Unit] = {
    val req = UpsertAllChampionStatisticsRequest(
      key = in.alexandriaKey.some,
      value = data.some
    )
    Task.deferFuture {
      alexandria.upsertAllChampionStatistics(req)
    }.map(_ => ())
  }

  def project(full: StoredAllChampionStatistics): AllChampionStatistics =
    full.value.get

}
