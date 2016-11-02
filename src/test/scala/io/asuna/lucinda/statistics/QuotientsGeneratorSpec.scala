package io.asuna.lucinda.statistics

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.{Sums, Quotients}

class QuotientsGeneratorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("generated quotients have properties set") {

    forAll { (sums: Sums) =>
      val quotients = QuotientsGenerator.generateQuotients(sums)

      if (!quotients.scalars.isDefined) {
        fail("Scalars not defined")
      }

      if (!quotients.deltas.isDefined) {
        fail("Deltas not defined")
      }

      if (!quotients.derivatives.isDefined) {
        fail("Derivatives not defined")
      }
    }
  }

  // TODO(igm): better tests
}
