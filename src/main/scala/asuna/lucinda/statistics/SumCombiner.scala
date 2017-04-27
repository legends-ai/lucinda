package asuna.lucinda.statistics

import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.AllChampionStatistics.Sums
import cats.implicits._

object SumCombiner {
  import SumsHelpers._
  import SumConverter._

  def combineSums(sums: Map[Int, MatchSum]): Sums = {
    val filtered = sums.filterNot { case (_, sum) =>
      sum.statistics.map(_.plays).orEmpty === 0
    }
    val aggsMap = filtered.map { case (champ, sums) =>
      sums.asAggregate(champ)
    }
    aggsMap.toList.combineAll
  }

}
