package io.asuna.lucinda

import io.asuna.lucinda.database.LucindaDatabase
import io.asuna.proto.lucinda.LucindaData._
import io.asuna.proto.service_lucinda.LucindaGrpc
import io.asuna.proto.service_lucinda.LucindaRpc._
import scala.concurrent.Future

class LucindaServer(db: LucindaDatabase) extends LucindaGrpc.Lucinda {

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

  override def getMatchSum(req: GetMatchSumRequest) = {
    db.matchSums.sum(req.filters.toSet)
  }

}
