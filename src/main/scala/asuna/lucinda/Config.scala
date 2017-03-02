package asuna.lucinda

import asuna.common.config.ConfigParser
import asuna.proto.league.Queue
import buildinfo.BuildInfo
import scala.concurrent.duration._

case class LucindaConfig(
  statisticsCacheExpiryMs: Duration = 15.minutes,
  allChampionStatisticsCacheExpiryMs: Duration = 15.minutes
) {
  val defaultQueues = Set(
    Queue.RANKED_FLEX_SR, // S7 Flex
    Queue.RANKED_SOLO_5x5, // S7 Solo
    Queue.TEAM_BUILDER_RANKED_SOLO // S6 Solo
  )
}

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  name = BuildInfo.name,
  version = BuildInfo.version,
  dependencies = Set("alexandria", "vulgate"),
  port = 45045,
  metaPort = 45046,
  initial = LucindaConfig()
) {

  opt[Int]("statistics_cache_expiry_ms").valueName("<milliseconds>")
    .action((x, c) => c.copy(service = c.service.copy(statisticsCacheExpiryMs = x.milliseconds)))
    .text("Milliseconds until the cache is expired for statistics")

  opt[Int]("all_champion_statistics_cache_expiry_ms").valueName("<milliseconds>")
    .action((x, c) => c.copy(service = c.service.copy(allChampionStatisticsCacheExpiryMs = x.milliseconds)))
    .text("Milliseconds until the cache is expired for all champion statistics statistics")

}
