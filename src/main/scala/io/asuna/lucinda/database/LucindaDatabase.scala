package io.asuna.lucinda.database

import io.asuna.lucinda.models.ConcreteMatchSumsModel
import com.websudos.phantom.dsl._
import io.asuna.common.Config
import io.asuna.lucinda.LucindaConfig

class LucindaDatabase(val keyspace: KeySpaceDef) extends Database[LucindaDatabase](keyspace) {

  object matchSums extends ConcreteMatchSumsModel with keyspace.Connector

}

object LucindaDatabase {

  def fromConfig(config: Config[LucindaConfig]): LucindaDatabase =
    new LucindaDatabase(
      ContactPoints(config.service.cassandraHosts)
        .keySpace(config.service.matchSumsKeyspace)
    )

}
