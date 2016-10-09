package io.asuna.lucinda

import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import scala.concurrent.Future

private class Server extends LucindaGrpc.Lucinda {

  override def getStatistics(req: GetStatisticsRequest) = {
    val reply = ChampionStatistics()
    Future.successful(reply)
  }

}
