package asuna.lucinda

import asuna.common.config.ConfigParser
import asuna.proto.league.Queue
import buildinfo.BuildInfo
import scala.concurrent.duration._

case class LucindaConfig(
  allChampionStatisticsDAOSettings: DAOSettings = DAOSettings("all-champion-statistics"),
  statisticsDAOSettings: DAOSettings = DAOSettings("statistics"),
) {
  val defaultQueues = Set(
    Queue.RANKED_FLEX_SR, // S7 Flex
    Queue.RANKED_SOLO_5x5, // S7 Solo
    Queue.TEAM_BUILDER_RANKED_SOLO // S6 Solo
  )
}

case class DAOSettings(
  name: String,
  bufferSize: Int = 200,
  refreshConcurrency: Int = 5,
  batchConcurrency: Int = 10,
  expiryTime: Duration = 15.minutes,
  cacheBypass: Boolean = false,
)

// TODO(igm): add flags for dao settings

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  name = BuildInfo.name,
  version = BuildInfo.version,
  dependencies = Set("alexandria", "vulgate"),
  port = 31310,
  metaPort = 31311,
  initial = LucindaConfig()
) {

  opt[Boolean]("cache_bypass").valueName("<bool>")
    .configure((x, c) => c.copy(
      allChampionStatisticsDAOSettings = c.allChampionStatisticsDAOSettings.copy(cacheBypass = x),
      statisticsDAOSettings = c.statisticsDAOSettings.copy(cacheBypass = x),
    ))
    .text("Whether to bypass the cache or not.")

}
