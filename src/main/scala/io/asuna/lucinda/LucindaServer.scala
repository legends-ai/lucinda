package io.asuna.lucinda

import io.grpc.{Server, ServerBuilder}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import java.util.logging.Logger
import scala.concurrent.{ ExecutionContext, Future }

object LucindaServer {

  private val logger = Logger.getLogger(classOf[LucindaServer].getName)

  def main(args: Array[String]): Unit = {
    val server = new LucindaServer(ExecutionContext.global)
    server.start()
    server.blockUntilShutdown()
  }

  // TODO(igm): make this port configurable
  private val port = 23981

}

class LucindaServer(executionContext: ExecutionContext) { self =>

  private[this] var server: Server = null

  private def start(): Unit = {
    server = ServerBuilder.forPort(LucindaServer.port).addService(LucindaGrpc.bindService(new LucindaImpl, executionContext)).build.start
    LucindaServer.logger.info("Server started, listening on " + LucindaServer.port)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        System.err.println("*** shutting down gRPC server since JVM is shutting down")
        self.stop()
        System.err.println("*** server shut down")
      }
    })
  }

  private def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  private def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }

  private class LucindaImpl extends LucindaGrpc.Lucinda {

    override def getStatistics(req: GetStatisticsRequest) = {
      val reply = ChampionStatistics()
      Future.successful(reply)
    }

  }

}
