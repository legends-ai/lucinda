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

}
