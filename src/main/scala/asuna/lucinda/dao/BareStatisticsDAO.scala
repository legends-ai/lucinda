package asuna.lucinda.dao

import asuna.lucinda.matches.StatisticsGenerator
import asuna.lucinda.DAOSettings
import asuna.lucinda.filters.MatchFilterSpaceHelpers
import asuna.lucinda.statistics.StatisticsAggregator
import asuna.proto.league._
import asuna.proto.league.alexandria.StoredStatistics
import asuna.proto.league.lucinda.{ Statistics, StatisticsKey }
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc._
import com.google.protobuf.timestamp.Timestamp
import cats.implicits._
import com.timgroup.statsd.NonBlockingStatsDClient
import monix.cats._
import monix.eval.Task


object BareStatisticsDAO {

  /**
    * @param patchNeighborhood Surrounding patches to display stats about.
    */
  case class Key(
    allChampions: Set[Int],
    patches: Set[String],
    patchNeighborhood: Seq[String],
    prevPatch: Option[String],

    champions: Set[Int],
    tiers: Set[Tier],
    regions: Set[Region],
    roles: Set[Role],
    enemies: Set[Int],
    queues: Set[Queue]
  ) {

    lazy val alexandriaKey: StatisticsKey = StatisticsKey(
      championIds = champions.toSeq,
      patches = patches.toSeq,
      tiers = tiers.toSeq,
      regions = regions.toSeq,
      roles = roles.toSeq,
      enemyIds = enemies.toSeq,
      queues = queues.toSeq
    )

    lazy val space = MatchFilterSpaceHelpers.generate(
      champions, patches, tiers, regions, enemies, roles, queues)

    lazy val byRoleFilters: Map[Role, MatchFiltersSpace] = (Role.values.toSet - Role.UNDEFINED_ROLE)
      .map(r => (r, r)).toMap
      .mapValues { someRole =>
        space.copy(roles = Seq(someRole))
      }

    lazy val byPatchFilters: Map[String, MatchFiltersSpace] = patchNeighborhood
      .map(p => (p, p)).toMap
      .mapValues { patch =>
        space.copy(versions = Seq(patch))
      }.toMap

    lazy val patchNbhdMap: Map[String, String] =
      patchNeighborhood.map(p => (p, p)).toMap
  }

}

/**
  * Fetcher for the bare statistics.
  */
class BareStatisticsDAO(settings: DAOSettings, alexandria: Alexandria, allChampionStatisticsDAO: AllChampionStatisticsDAO, statsd: NonBlockingStatsDClient) extends RefreshableProtoDAO[
  BareStatisticsDAO.Key,
  StoredStatistics,
  Statistics
](settings) {
  import BareStatisticsDAO.Key

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

  def compute(in: Key): Task[Statistics] = {
    for {
      // Role information
      byRole <- in.byRoleFilters.traverse { subspace =>
        Task.deferFuture {
          alexandria.getSum(GetSumRequest(space = subspace.some))
        }
      }

      // Patch information
      byPatch <- in.byPatchFilters.traverse { subspace =>
        Task.deferFuture {
          alexandria.getSum(GetSumRequest(space = subspace.some))
        }
      }

      // Stats (where Statistic objects come from)
      allStats <- allChampionStatisticsDAO.get(
        in.allChampions,
        in.tiers,
        in.patches,
        in.prevPatch,
        in.regions,
        in.roles,
        in.queues,
        in.enemies
      )

      // TODO(igm): reuse prev call data
      // This contains an element of the form Map[String, AllChampionStatistics]
      // where key is the patch and value is the stats.
      patchNbhd <- in.patchNbhdMap.traverse { patch =>
        allChampionStatisticsDAO.get(
          in.allChampions,
          in.tiers,
          Set(patch),
          None,
          in.regions,
          in.roles,
          in.queues,
          in.enemies
        )
      }

      stats = StatisticsGenerator.makeStatistics(
        champions = in.champions,
        allStats = allStats,
        patchNbhd = patchNbhd,
        roles = in.roles,
        byRole = byRole,
        byPatch = byPatch,
        patches = in.patches
      )

      _ = statsd.increment("generate_statistics")
    } yield stats
  }

}
