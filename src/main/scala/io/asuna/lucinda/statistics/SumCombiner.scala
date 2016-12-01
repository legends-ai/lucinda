package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.Sums
import io.asuna.proto.match_sum.MatchSum
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
