package io.asuna.lucinda.database

import io.asuna.lucinda.Config
import com.websudos.phantom.dsl._

object Connector {

  def fromConfig(config: Config): KeySpaceDef =
    ContactPoints(config.cassandraHosts).keySpace("athena_out")

}
