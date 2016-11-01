package io.asuna.lucinda.statistics

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

import io.asuna.proto.enums.Role

class StatisticsAggregatorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("preservation of role") {
    forAll { (role: Role) =>
      val stats = StatisticsAggregator.makeStatistics(role, Map())
      stats.role should be (role)
    }
  }

}
