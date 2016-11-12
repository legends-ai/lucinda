package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.Statistic
import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.{Results, Quotients}

class ResultsGeneratorSpec extends PropSpec
    with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("generated results have unique rankings") {

    forAll { (quotients: Quotients) =>
      val results = ResultsGenerator.generateResults(quotients)

      testStatsMap(results.scalars.get.wins)
    }
  }

  def testStatsMap(stats: Map[Int, Statistic]) = {
    // Unique ranking
    val statRanks = stats.values.map(_.rank)
    assert(statRanks.toSet.size == statRanks.size)

    // Ranking is from 1 to n
    statRanks.foreach { rank =>
      assert(rank <= statRanks.size)
    }

    // Percentile should be in [0, 1]
    val percentiles = stats.values.map(_.percentile)
    percentiles.foreach { p =>
      assert(p >= 0 && p <= 1)
    }

    // Uniqueness of percentile
    assert(percentiles.toSet.size == percentiles.size)

    // Average should be global
    // can be zero if there are no members of the set!
    assert(stats.values.map(_.average).toSet.size <= 1)

    // validity of ranks
    stats.values.zip(stats.values).foreach { case (a, b) =>
      if (a.rank < b.rank) {
        assert(a.value > b.value)
        assert(a.percentile > b.percentile)
      } else if (a.rank > b.rank) {
        assert(a.value < b.value)
        assert(a.percentile < b.percentile)
      }
    }

    // TODO(igm): test change when it happens
  }

  // TODO(igm): better tests
}
