package io.asuna.lucinda.statistics

import io.asuna.lucinda.FutureUtil
import io.asuna.proto.enums.{Region, Role}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics._
import io.asuna.lucinda.database.LucindaDatabase
import scala.concurrent.{ ExecutionContext, Future }

object StatisticsAggregator {

  /**
    *  This function derives a ChampionStatistics object from a set of patches, tiers, a region, and an enemy.
    *
    *  An overview of the steps to do this is as follows:
    *  1. Find filters for each champion. (see buildFilterSet)
    *  2. Convert these filters into MatchSums using the database.
    *  3. Build the Statistics objects from the raw MatchSums. (see makeStatistics)
    *
    *  This does not take caching into account.
    */
  def aggregate(
    champions: Set[Int], patches: Set[String], tiers: Set[Int], region: Region, enemy: Int = -1
  )(implicit db: LucindaDatabase, ec: ExecutionContext): Future[ChampionStatistics] = {
    // A lot goes on in this function, especially since we're dealing with Futures.
    // I'll try to explain every step in detail.

    // First, we're going to iterate over every Role. This is a given --
    // the ChampionStatistics returns one Statistics object for each role.
    val statsFuts = Role.values.map { role =>
      // Here, we build the Set[MatchFilters] for every champion.
      // This is of type Map[Int, Set[MatchFilters]].
      val filtersMap = champions.map {champ =>
        (champ, buildFilterSet(champ, patches, tiers, region, role, enemy))
      }.toMap

      // Next, we'll compute the MatchSums. This is where the function is no longer
      // pure, and we make a database call. (Note that since we're using futures,
      // no database call is made at the time of execution.) This returns a
      // Map[Int, Future[MatchSum]].
      val sumsMapFuts = filtersMap.mapValues(filters => db.matchSums.sum(filters))

      // Next, we'll extract the Future from the value using some map magic.
      // Thus we end up with a Future[Map[Int, MatchSum]].
      val sumsMapFut = FutureUtil.sequenceMap(sumsMapFuts)

      // Finally, we'll map over the values of this map to generate a Statistics
      // object for each value. Thus we end up with a Future[Statistics].
      sumsMapFut.map { sumsMap =>
        makeStatistics(role, sumsMap)
      }
    }

    // Now that we have a List[Future[Statistics]], we'll again unwrap the Future
    // to create a Future[ChampionStatistics], and we are done.
    Future.sequence(statsFuts).map(s => ChampionStatistics(statistics = s))
  }

  def buildFilterSet(
    championId: Int, patches: Set[String], tiers: Set[Int],
    region: Region, role: Role, enemy: Int = -1
  ): Set[MatchFilters] = {
    for {
      patch <- patches
      tier <- tiers
    } yield MatchFilters(
      championId = championId,
      patch = patch,
      tier = tier,
      region = region,
      enemyId = enemy,
      role = role
    )
  }

  /**
    * Make the Statistics object from a role and sums for that role. This function is pure.
    */
  def makeStatistics(role: Role, rawSums: Map[Int, MatchSum]): Statistics = {
    val sums = SumCombiner.combineSums(rawSums)
    val quotients = QuotientsGenerator.generateQuotients(sums)
    val results = ResultsGenerator.generateResults(quotients)
    Statistics(
      role = role,
      results = Some(results),
      quotients = Some(quotients),
      sums = Some(sums)
    )
  }

}
