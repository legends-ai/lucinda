package io.asuna.lucinda

import io.asuna.asunasan.{ AsunaServiceType, Config, ConfigParser }

case class LucindaConfig(
  cassandraHosts: Seq[String] = List("localhost"),
  redisHost: String = "localhost",
  redisPort: Int = 6379
)

object LucindaConfig {

  // TODO(igm): figure out how to use SBT=specified version here
  val parser = new ConfigParser[LucindaConfig](AsunaServiceType.Lucinda, "0.1.0", Set(AsunaServiceType.Vulgate)) {

    opt[Seq[String]]("cassandra_hosts").valueName("<node1>,<node2>...")
      .action((x, c) => c.copy(custom = c.custom.copy(cassandraHosts = x)))
      .text("List of Cassandra hosts to connect to.")

    opt[String]("redis_host").valueName("<host>")
      .action((x, c) => c.copy(custom = c.custom.copy(redisHost = x)))
      .text("Redis host to connect to.")

    opt[Int]("redis_port").valueName("<port>")
      .action((x, c) => c.copy(custom = c.custom.copy(redisPort = x)))
      .text("Redis port to connect to.")

  }

  def parse(args: Seq[String]): Option[Config[LucindaConfig]] = {
    parser.parseWithDefaults(args, LucindaConfig())
  }

}
