package asuna.lucinda.matches

import asuna.lucinda.statistics.{ MatchAggregatorArgs, MatchSumGeneratorHelper }
import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

class MatchAggregatorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("preservation of role") {
    forAll { (args: MatchAggregatorArgs) =>
      val result = MatchAggregator.makeAggregate(args.champion, args.minPlayRate, args.patchStats, args.byRole, args.byPatch)
    }
  }

}
