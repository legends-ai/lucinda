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
      (championId: Int, patch: String, tiers: Set[Int],
       region: Region, role: Role, enemy: Int) =>

      val result = buildFilterSet(championId, patch, tiers, region, role, enemy)
      result.size should be (tiers.size)
    }
  }

  property("correct # of filters for each property") {
    forAll {
      (championId: Int, patch: String, tiers: Set[Int],
       region: Region, role: Role, enemy: Int) =>

      val result = buildFilterSet(championId, patch, tiers, region, role, enemy)

      // constants
      result.filter(_.championId == championId).size should be (result.size)
      result.filter(_.region == region).size should be (result.size)
      result.filter(_.enemyId == enemy).size should be (result.size)
      result.filter(_.role == role).size should be (result.size)
      result.filter(_.patch == patch).size should be (result.size)

      result.groupBy(_.tier).values.foreach { group =>
        group.size should be (result.size / tiers.size)
      }
    }
  }

  property("correct # of classes of filters for each property") {
    forAll {
      (championId: Int, patch: String, tiers: Set[Int],
      region: Region, role: Role, enemy: Int) =>

      val result = buildFilterSet(championId, patch, tiers, region, role, enemy)

      whenever (!tiers.isEmpty) {
        result.groupBy(_.championId).size should be (1)
        result.groupBy(_.region).size should be (1)
        result.groupBy(_.enemyId).size should be (1)
        result.groupBy(_.role).size should be (1)
        result.groupBy(_.patch).size should be (1)
        result.groupBy(_.tier).size should be (tiers.size)
      }
    }
  }

  property("Empty result for empty tiers") {
    forAll {
      (championId: Int, patch: String, region: Region, role: Role, enemy: Int) =>
      val result = buildFilterSet(championId, patch, Set(), region, role, enemy)
      result.size should be (0)
    }
  }

}
