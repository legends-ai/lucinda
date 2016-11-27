package io.asuna.lucinda.database

import com.websudos.phantom.dsl._
import io.asuna.asunasan.Config
import io.asuna.lucinda.LucindaConfig

object Connector {

  def fromConfig(config: Config[LucindaConfig]): KeySpaceDef =
    ContactPoints(config.service.cassandraHosts).keySpace("athena_out")

}
