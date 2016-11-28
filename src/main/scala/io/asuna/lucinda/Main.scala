package io.asuna.lucinda

import io.grpc.{Server, ServerBuilder}
import io.asuna.asunasan.Config
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import io.asuna.proto.match_sum.MatchSum
import io.asuna.lucinda.database.{ Connector, LucindaDatabase }
import java.util.logging.Logger
import scala.concurrent.{ ExecutionContext, Future }

object Main {

  def main(args: Array[String]): Unit = {
    new LucindaServer(args).standReady()
  }

}
