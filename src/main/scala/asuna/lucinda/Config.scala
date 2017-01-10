package asuna.lucinda

import buildinfo.BuildInfo
import asuna.common.{ Config, ConfigParser }

case class LucindaConfig(
  matchSumsKeyspace: String = "bacchus_dev",
  cassandraHosts: Seq[String] = List("localhost"),
  redisHost: String = "localhost",
  redisPort: Int = 6379
)

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  myService = BuildInfo.name,
  dependencies = Set("vulgate"),
  port = 45045,
  healthPort = 45046,
  initial = LucindaConfig()
) {

  opt[Seq[String]]("cassandra_hosts").valueName("<node1>,<node2>...")
    .action((x, c) => c.copy(service = c.service.copy(cassandraHosts = x)))
    .text("List of Cassandra hosts to connect to.")

  opt[String]("match_sums_keyspace").valueName("<host>")
    .action((x, c) => c.copy(service = c.service.copy(matchSumsKeyspace = x)))
    .text("Match sums keyspace to connect to.")

  opt[String]("redis_host").valueName("<host>")
    .action((x, c) => c.copy(service = c.service.copy(redisHost = x)))
    .text("Redis host to connect to.")

  opt[Int]("redis_port").valueName("<port>")
    .action((x, c) => c.copy(service = c.service.copy(redisPort = x)))
    .text("Redis port to connect to.")

}
