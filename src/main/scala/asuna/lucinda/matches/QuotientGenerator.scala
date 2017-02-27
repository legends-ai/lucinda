package asuna.lucinda.matches

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import MatchQuotient._
import MatchQuotient.Collections._
import cats.implicits._

object QuotientGenerator {

  def generate(sum: MatchSum): MatchQuotient = {
    import Divisor._
    import PathMerger._
    import SubscalarMapping._
    sum
      .copy(collections = sum.collections.map { colls =>
        colls.copy(
          skillOrders = colls.skillOrders.mergePaths,
          coreBuilds = colls.coreBuilds.mergePaths
        )
      })
      .quotient
  }

}
