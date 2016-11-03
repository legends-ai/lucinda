package io.asuna.lucinda.statistics

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

import io.asuna.proto.enums.{Region, Role}
import StatisticsAggregator._

class StatisticsAggregatorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("preservation of role") {
    forAll { (role: Role) =>
      val stats = makeStatistics(role, Map())
      stats.role should be (role)
    }
  }

  property("filters should be correct cardinality") {
    forAll {
      (championId: Int, patches: Set[String], tiers: Set[Int],
       region: Region, enemy: Int) =>

      val result = buildFilterSet(championId, patches, tiers, region, enemy)
      result.size should be (patches.size * tiers.size * Role.values.size)
    }
  }

  property("correct # of filters for each property") {
    forAll {
      (championId: Int, patches: Set[String], tiers: Set[Int],
       region: Region, enemy: Int) =>

      val result = buildFilterSet(championId, patches, tiers, region, enemy)

      // constants
      result.filter(_.championId == championId).size should be (result.size)
      result.filter(_.region == region).size should be (result.size)
      result.filter(_.enemyId == enemy).size should be (result.size)

      result.groupBy(_.patch).values.foreach { group =>
        group.size should be (result.size / patches.size)
      }
      result.groupBy(_.tier).values.foreach { group =>
        group.size should be (result.size / tiers.size)
      }
      result.groupBy(_.role).values.foreach { group =>
        group.size should be (result.size / Role.values.size)
      }
    }
  }

  property("correct # of classes of filters for each property") {
    forAll {
      (championId: Int, patches: Set[String], tiers: Set[Int],
      region: Region, enemy: Int) =>

      val result = buildFilterSet(championId, patches, tiers, region, enemy)

      whenever (!patches.isEmpty && !tiers.isEmpty) {
        result.groupBy(_.championId).size should be (1)
        result.groupBy(_.region).size should be (1)
        result.groupBy(_.enemyId).size should be (1)
        result.groupBy(_.patch).size should be (patches.size)
        result.groupBy(_.tier).size should be (tiers.size)
        result.groupBy(_.role).size should be (Role.values.size)
      }
    }
  }

  property("Empty result for empty patches or tiers") {
    forAll {
      (championId: Int, tiers: Set[Int], region: Region, enemy: Int) =>
      val result = buildFilterSet(championId, Set(), tiers, region, enemy)
      result.size should be (0)
    }
    forAll {
      (championId: Int, patches: Set[String], region: Region, enemy: Int) =>
      val result = buildFilterSet(championId, patches, Set(), region, enemy)
      result.size should be (0)
    }
  }

}
