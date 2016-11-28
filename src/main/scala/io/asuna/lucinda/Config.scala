package io.asuna.lucinda

import io.asuna.asunasan.{ AsunaServiceType, Config, ConfigParser }

case class LucindaConfig(
  cassandraHosts: Seq[String] = List("localhost"),
  redisHost: String = "localhost",
  redisPort: Int = 6379
)

object LucindaConfigParser extends ConfigParser[LucindaConfig](
  myService = AsunaServiceType.Lucinda,
  version = "0.1.0",
  dependencies = Set(AsunaServiceType.Vulgate),
  port = 45045,
  healthPort = 45046,
  initial = LucindaConfig()
) {

  opt[Seq[String]]("cassandra_hosts").valueName("<node1>,<node2>...")
    .action((x, c) => c.copy(service = c.service.copy(cassandraHosts = x)))
    .text("List of Cassandra hosts to connect to.")

  opt[String]("redis_host").valueName("<host>")
    .action((x, c) => c.copy(service = c.service.copy(redisHost = x)))
    .text("Redis host to connect to.")

  opt[Int]("redis_port").valueName("<port>")
    .action((x, c) => c.copy(service = c.service.copy(redisPort = x)))
    .text("Redis port to connect to.")

}
