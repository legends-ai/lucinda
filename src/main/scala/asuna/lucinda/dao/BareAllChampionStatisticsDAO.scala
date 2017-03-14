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
import com.timgroup.statsd.NonBlockingStatsDClient
import monix.cats._
import monix.eval.Task


object BareAllChampionStatisticsDAO {

  case class Key(
    allChampions: Set[Int],
    tiers: Set[Tier],
    patches: Set[String],
    regions: Set[Region],
    roles: Set[Role],
    queues: Set[Queue],
    enemies: Set[Int],
    reverse: Boolean = false
  ) {

    /**
      * Map of every champion to the filters space.
      */
    lazy val filtersMap: Map[Int, MatchFiltersSpace] = allChampions
      .map(c => (c, c)).toMap
      .mapValues { champ =>
        val basis = MatchFilterSpaceHelpers.generate(
          Set(champ), patches, tiers, regions, enemies, roles, queues
        )
        if (reverse) {
          basis.copy(championIds = basis.enemyIds, enemyIds = basis.championIds)
        } else {
          basis
        }
      }

    lazy val alexandriaKey: AllChampionStatisticsKey = AllChampionStatisticsKey(
      tiers = tiers.toSeq,
      patches = patches.toSeq,
      regions = regions.toSeq,
      roles = roles.toSeq,
      enemyIds = enemies.toSeq,
      queues = queues.toSeq,
      reverse = reverse
    )

  }

}

/**
  * Fetcher for the bare statistics.
  */
class BareAllChampionStatisticsDAO(settings: DAOSettings, alexandria: Alexandria, statsd: NonBlockingStatsDClient) extends RefreshableProtoDAO[
  BareAllChampionStatisticsDAO.Key,
  StoredAllChampionStatistics,
  AllChampionStatistics
](settings) {
  import BareAllChampionStatisticsDAO.Key

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

  /**
    *  This function derives a AllChampionStatistics object from a patch, a set of tiers, a region, and an enemy.
    *
    *  An overview of the steps to do this is as follows:
    *  1. Find filters for each champion. (see buildFilterSet)
    *  2. Convert these filters into MatchSums using the database.
    *  3. Build the Statistics objects from the raw MatchSums. (see makeStatistics)
    *
    *  This does not take caching into account.
    */
  def compute(in: Key): Task[AllChampionStatistics] = {
    for {
      // Here, we'll compute the MatchSums. This is where the function is no longer
      // pure, and we make a database call. (Note that since we're using futures,
      // no database call is made at the time of execution.) This returns a
      // Map[Int, Task[MatchSum]].
      sumsMap <- in.filtersMap.traverse { space =>
        Task.deferFuture(alexandria.getSum(GetSumRequest(space = space.some)))
      }

      // Finally, we'll map over the values of this map to generate a Statistics
      // object for each value. Thus we end up with a Future[AllChampionStatistics],
      // and we are done.
      stats = StatisticsAggregator.makeStatistics(sumsMap)
      _ = statsd.increment("generate_all_champion_statistics")
    } yield stats
  }

}
