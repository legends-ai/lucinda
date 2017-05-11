package asuna.lucinda.statistics

import scala.collection.immutable.Map

import asuna.proto.league.lucinda.{ AllChampionStatistics, Statistic }
import cats.syntax.semigroup._
import cats.instances.option._
import cats.derived._, semigroup._, legacy._
import cats.Semigroup

object ChangeMarker {

  implicit object markStatisticSemigroup extends Semigroup[Statistic] {
    def combine(stat: Statistic, prev: Statistic): Statistic = {
      stat.copy(
        changeInRank = stat.rank - prev.rank,
        changeInValue = stat.mean - prev.mean,
      )
    }
  }

  implicit object markMapSemigroup extends Semigroup[Map[Int, Statistic]] {
    def combine(stat: Map[Int, Statistic], prev: Map[Int, Statistic]): Map[Int, Statistic] = {
      stat.transform { (key, value) =>
        prev.get(key) match {
          case Some(v) => value |+| v
          case None => value
        }
      }
    }
  }

  implicit object markDragonSemigroup extends Semigroup[Seq[AllChampionStatistics.Results.Scalars.DragonStat]] {

    def combine(
      cur: Seq[AllChampionStatistics.Results.Scalars.DragonStat],
      prev: Seq[AllChampionStatistics.Results.Scalars.DragonStat]
    ): Seq[AllChampionStatistics.Results.Scalars.DragonStat] = {
      cur.map { stat =>
        prev.find(_.dragon == stat.dragon) match {
          case Some(other) => stat.copy(value = stat.value |+| other.value)
          case None => stat
        }
      }
    }

  }

  implicit val resultsSemigroup = Semigroup[AllChampionStatistics.Results]

  def mark(stats: AllChampionStatistics, prev: AllChampionStatistics): AllChampionStatistics = {
    stats.copy(
      results = stats.results |+| prev.results,
    )
  }

}
