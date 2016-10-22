package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData.ChampionStatistics.{Results, Quotients}

/**
  * Generats the Results part of the statistics.
  */
object ResultsGenerator {

  def generateResults(quotients: Quotients): Results = {
    Results()
  }

  def makeStat(statsMap: Map[Int, Double]): Map[Int, Results.Statistic] = {
    val sortedPairs = statsMap.toSeq.sortBy(_._2)

    // average of the value
    val average =  statsMap.size match {
      case 0 => 0
      case _ => statsMap.values.sum / statsMap.values.size
    }

    val statsWithIndex = sortedPairs.zipWithIndex.map { case ((champ, value), index) =>
      (champ, (value, index))
    }.toMap

    statsWithIndex.mapValues { case (value, index) =>
      Results.Statistic(
        rank = index + 1,
        value = value,
        average = average,
        percentile = 1 - index / statsMap.size
      )
    }
  }

}
