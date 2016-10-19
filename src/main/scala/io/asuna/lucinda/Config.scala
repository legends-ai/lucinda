package io.asuna.lucinda

import scopt.OptionParser

case class Config(
  cassandraHosts: Seq[String] = List("localhost:9042"),
  port: Int = 23981
)

object Config {
  /** Parser for command line options. */
  val parser = new OptionParser[Config]("lucinda") {
    head("lucinda", "0.1.0")

    // Our list of Cassandra hosts.
    opt[Seq[String]]("cassandraHosts").valueName("<node1>,<node2>...")
      .action((x, c) => c.copy(cassandraHosts = x))
      .text("List of Cassandra hosts to connect to.")

    opt[Int]("port").valueName("<port>")
      .action((x, c) => c.copy(port = x))
      .text("The port on which gRPC should listen.")
  }

  def parse(args: Seq[String]): Option[Config] = {
    parser.parse(args, Config())
  }

}
