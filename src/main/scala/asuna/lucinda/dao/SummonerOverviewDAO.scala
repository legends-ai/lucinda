package asuna.lucinda.dao

import cats.implicits._
import asuna.proto.league._
import asuna.proto.league.lucinda._
import monix.eval.Task

object SummonerOverviewDAO {

  case class Key(
    id: SummonerId,
    allChampions: Set[Int],
    prevPatch: Option[String],
    roles: Set[Role],
    patches: Set[String],
    queues: Set[Queue],
    enemyChampionIds: Set[Int]
  )

}

class SummonerOverviewDAO(summonerChampions: SummonerChampionsDAO)
  extends EphemeralDAO[SummonerOverviewDAO.Key, SummonerOverview] {

  import SummonerOverviewDAO._

  def compute(key: Key): Task[SummonerOverview] = {
    summonerChampions.get(
      id = key.id,
      allChampions = key.allChampions,
      prevPatch = key.prevPatch,
      roles = key.roles,
      patches = key.patches,
      queues = key.queues,
      enemyIds = key.enemyChampionIds
    ).map { scs =>
      (scs.results.flatMap(_.scalars) |@| scs.sums.flatMap(_.scalars) |@| scs.sums.map(_.plays)).map {
        (results, sums, plays) =>
          SummonerOverview(
            plays = plays.values.sum,
            wins = sums.wins.values.map(_.sum).sum.toLong,

            // kda
            kills = sums.kills.values.map(_.sum).sum.toLong,
            deaths = sums.deaths.values.map(_.sum).sum.toLong,
            assists = sums.assists.values.map(_.sum).sum.toLong,

            // champion data
            championOverviews = key.allChampions.toList
              .map(c => c -> plays.get(c).orEmpty)
              // top 10 champions. TODO(igm): configurable? in request?
              .filter(_._2 > 0).sortBy(_._2).reverse.take(10)
              .map { case (champ, _) =>
                SummonerOverview.ChampionOverview(
                  id = champ,
                  plays = plays.get(champ).orEmpty,
                  wins = results.wins.get(champ),
                  kills = results.kills.get(champ),
                  deaths = results.deaths.get(champ),
                  assists = results.assists.get(champ),
                  minionsKilled = results.minionsKilled.get(champ),
                  firstBlood = results.firstBlood.get(champ)
                )
              }
          )
      }.getOrElse(SummonerOverview())
    }
  }

}
