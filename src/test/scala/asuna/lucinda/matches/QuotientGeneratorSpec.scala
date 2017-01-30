package asuna.lucinda.matches

import asuna.lucinda.statistics.MatchSumGeneratorHelper
import asuna.proto.league.MatchSum
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

class QuotientGeneratorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  val orderGen = Gen.choose(1, 18) flatMap { ct =>
    Gen.listOfN(ct, Gen.oneOf("Q", "W", "E", "R")).map(_.mkString)
  }

  val orderPairGen = orderGen flatMap { order =>
    genSubscalars.map((order, _))
  }

  val ordersGen = Gen.mapOf(orderPairGen)

  implicit lazy val arbOrdersGen: Arbitrary[Map[String, MatchSum.Subscalars]] = Arbitrary(ordersGen)

  property("merged skill orders merge properly") {

    forAll { (skillOrders: Map[String, MatchSum.Subscalars]) =>
      val merged = QuotientGenerator.mergeSkillOrders(skillOrders)

      merged.size should be (skillOrders.filterKeys(_.length() == 18).size)

      // All merged keys should be present in skillOrders.
      val mergedKeys = merged.keys.toSet
      skillOrders.keys.toSet.intersect(mergedKeys) should be (mergedKeys)
    }

  }

}
