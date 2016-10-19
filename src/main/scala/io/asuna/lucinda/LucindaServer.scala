package io.asuna.lucinda

import io.grpc.{Server, ServerBuilder}
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import java.util.logging.Logger
import scala.concurrent.{ ExecutionContext, Future }

object LucindaServer {

  private lazy val logger = Logger.getLogger(classOf[LucindaServer].getName)

  def main(args: Array[String]): Unit = {
    // Parse and verify config
    val config = Config.parse(args)
    if (!config.isDefined) {
      sys.exit(1)
      return
    }

    val server = new LucindaServer(config.get, ExecutionContext.global)
    server.start()
    server.blockUntilShutdown()
  }

}

class LucindaServer(config: Config, executionContext: ExecutionContext) { self =>

  private[this] var server: Server = null

  private def start(): Unit = {
    server = ServerBuilder.forPort(config.port).addService(LucindaGrpc.bindService(new LucindaImpl, executionContext)).build.start
    LucindaServer.logger.info("Server started, listening on " + config.port)
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

    override def getChampion(req: GetChampionRequest) = {
      Future.successful(Champion())
    }

    override def getMatchup(req: GetMatchupRequest) = {
      Future.successful(Matchup())
    }

  }

}
