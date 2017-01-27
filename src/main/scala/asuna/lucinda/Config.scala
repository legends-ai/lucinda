package asuna.lucinda

import asuna.proto.enums.QueueType
import buildinfo.BuildInfo
import asuna.common.config.ConfigParser

case class LucindaConfig(
  redisHost: String = "localhost",
  redisPort: Int = 6379
) {
  val defaultQueues = List(
    QueueType.RANKED_FLEX_SR, // S7 Flex
    QueueType.RANKED_SOLO_5x5, // S7 Solo
    QueueType.TEAM_BUILDER_RANKED_SOLO // S6 Solo
  )
}

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  name = BuildInfo.name,
  version = BuildInfo.version,
  dependencies = Set("alexandria", "vulgate"),
  port = 45045,
  healthPort = 45046,
  initial = LucindaConfig()
) {

  opt[String]("redis_host").valueName("<host>")
    .action((x, c) => c.copy(service = c.service.copy(redisHost = x)))
    .text("Redis host to connect to.")

  opt[Int]("redis_port").valueName("<port>")
    .action((x, c) => c.copy(service = c.service.copy(redisPort = x)))
    .text("Redis port to connect to.")

}
