package asuna.lucinda.statistics

import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.ChampionStatistics.Sums
import cats.implicits._

import SumsHelpers._
import SumConversionHelpers._

object SumCombiner {

  def combineSums(sums: Map[Int, MatchSum]): Sums = {
    val filtered = sums.filterNot { case (_, sum) =>
      sum.scalars.map(_.plays).getOrElse(0L) == 0L
    }
    val aggsMap = filtered.map { case (champ, sums) =>
      sums.asAggregate(champ)
    }
    aggsMap.toList.combineAll
  }

}
