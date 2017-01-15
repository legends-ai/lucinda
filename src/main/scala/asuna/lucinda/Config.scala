package asuna.lucinda

import asuna.proto.enums.QueueType
import buildinfo.BuildInfo
import asuna.common.{ Config, ConfigParser }

case class LucindaConfig(
  redisHost: String = "localhost",
  redisPort: Int = 6379
) {

  val defaultQueues = Set(QueueType.RANKED_FLEX_SR, QueueType.TEAM_BUILDER_DRAFT_RANKED_5x5)

}

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  myService = BuildInfo.name,
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
