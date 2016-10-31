package io.asuna.lucinda.statistics

import org.scalatest._
import org.scalacheck._
import prop._
import scala.collection.immutable._
import io.asuna.proto.enums.Role

class StatisticsAggregatorSpec extends PropSpec with PropertyChecks with Matchers {

  implicit lazy val arbRole: Arbitrary[Role] = Arbitrary(Gen.oneOf(Role.values))

  property("should generate the correct role") {
    forAll { (role: Role) =>
      val stats = StatisticsAggregator.makeStatistics(role, Map())
      stats.role should be (role)
    }
  }

}
