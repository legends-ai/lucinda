package io.asuna.lucinda

import io.asuna.asunasan.{ Config, ConfigParser }

case class LucindaConfig(
  cassandraHosts: Seq[String] = List("localhost")
)

object LucindaConfig {

  val parser = new ConfigParser[LucindaConfig]("lucinda") {
    // Our list of Cassandra hosts.
    opt[Seq[String]]("cassandraHosts").valueName("<node1>,<node2>...")
      .action((x, c) => c.copy(custom = c.custom.copy(cassandraHosts = x)))
      .text("List of Cassandra hosts to connect to.")
  }

  def parse(args: Seq[String]): Option[Config[LucindaConfig]] = {
    parser.parseWithDefaults(args, LucindaConfig())
  }

}
