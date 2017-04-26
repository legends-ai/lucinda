package asuna.lucinda.matches

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import MatchQuotient.Collections._
import cats.implicits._
import asuna.proto.league.MatchSum.Collections.Trinket

object QuotientGenerator {

  def generate(sum: MatchSum): MatchQuotient = {
    import Divisible._
    import Divisor._
    import PathMerger._
    import SubscalarsMapping._
    sum
      .copy(collections = sum.collections.map { colls =>
        colls.copy(
          startingTrinkets = removeZeroTrinket(colls.startingTrinkets),
          skillOrders = colls.skillOrders.mergePaths,
          coreBuilds = colls.coreBuilds.mergePaths,
        )
      })
      .quotient
  }

  val defaultTrinket = 3340

  private def removeZeroTrinket(trinkets: Seq[Trinket]): Seq[Trinket] =  {
    val (zero, nonzero) = trinkets.toList.partition(_.trinket === 0)
    nonzero.map { trinket =>
      if (trinket.trinket === defaultTrinket) {
        trinket.copy(subscalars = trinket.subscalars |+| zero.map(_.subscalars).combineAll)
      } else {
        trinket
      }
    }
  }

}
