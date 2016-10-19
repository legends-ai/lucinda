package io.asuna.lucinda.database

import io.asuna.lucinda.models.ConcreteMatchSumsModel
import com.websudos.phantom.dsl._

class LucindaDatabase(val keyspace: KeySpaceDef) extends Database[LucindaDatabase](keyspace) {

  object matchSums extends ConcreteMatchSumsModel with keyspace.Connector

}
