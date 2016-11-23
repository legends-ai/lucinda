package io.asuna.lucinda

import io.asuna.asunasan.{ AsunaServiceType, Config, ConfigParser }

case class LucindaConfig(
  cassandraHosts: Seq[String] = List("localhost")
)

object LucindaConfig {

  // TODO(igm): figure out how to use SBT=specified version here
  val parser = new ConfigParser[LucindaConfig](AsunaServiceType.Lucinda, "0.1.0", Set(AsunaServiceType.Vulgate)) {
    // Our list of Cassandra hosts.
    opt[Seq[String]]("cassandraHosts").valueName("<node1>,<node2>...")
      .action((x, c) => c.copy(custom = c.custom.copy(cassandraHosts = x)))
      .text("List of Cassandra hosts to connect to.")
  }

  def parse(args: Seq[String]): Option[Config[LucindaConfig]] = {
    parser.parseWithDefaults(args, LucindaConfig())
  }

}
