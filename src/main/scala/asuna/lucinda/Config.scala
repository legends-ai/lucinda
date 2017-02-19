package asuna.lucinda

import asuna.common.config.ConfigParser
import asuna.proto.league.Queue
import buildinfo.BuildInfo

case class LucindaConfig() {
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
)
