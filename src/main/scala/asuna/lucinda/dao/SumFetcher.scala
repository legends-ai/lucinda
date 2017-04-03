package asuna.lucinda.dao

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.SummonerId
import asuna.proto.league.alexandria.AlexandriaGrpc.Alexandria
import asuna.proto.league.alexandria.rpc.{ GetSumRequest, GetSummonerMatchSumRequest }
import asuna.proto.league.{ MatchFiltersSpace, MatchSum }
import monix.eval.Task
import cats.implicits._


trait SumFetcher[-K] {
  def fetchSums(ctx: K, space: MatchFiltersSpace): Task[MatchSum]
}

class SummonerSumFetcher(alexandria: Alexandria) extends SumFetcher[SummonerKey] {
  def fetchSums(ctx: SummonerKey, space: MatchFiltersSpace): Task[MatchSum] = {
    Task.deferFuture {
      val req = GetSummonerMatchSumRequest(
        summoner = ctx.id.some,
        space = space.some
      )
      alexandria.getSummonerMatchSum(req)
    }.map(_.matchSum.orEmpty)
  }
}

class AllSumFetcher(alexandria: Alexandria) extends SumFetcher[AllKey] {
  def fetchSums(ctx: AllKey, space: MatchFiltersSpace): Task[MatchSum] = {
    Task.deferFuture {
      println(s"gimme some space")
      alexandria.getSum(GetSumRequest(space = space.some))
    }.map { x =>
      println("done")
      x
    }
  }
}

trait SummonerKey {
  def id: SummonerId
}

trait AllKey
