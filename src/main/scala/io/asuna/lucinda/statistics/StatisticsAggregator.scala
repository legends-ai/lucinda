package io.asuna.lucinda.statistics

import io.asuna.proto.lucinda.LucindaData._
import io.asuna.lucinda.database.LucindaDatabase

object StatisticsAggregator {

  def aggregate()(implicit db: LucindaDatabase): ChampionStatistics = {
    return ChampionStatistics()
  }

}
