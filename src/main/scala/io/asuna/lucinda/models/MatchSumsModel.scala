package io.asuna.lucinda.models

import com.websudos.phantom.dsl._
import com.google.protobuf.CodedInputStream
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.match_filters.MatchFilters
import io.asuna.asunasan.legends.MatchSumOperators._
import scala.concurrent.Future

abstract class MatchSumsModel extends CassandraTable[ConcreteMatchSumsModel, MatchSum] {

  object championId extends IntColumn(this) with PartitionKey[Int] {
    override def name = "champion_id"
  }

  object enemyId extends IntColumn(this) with PartitionKey[Int] {
    override def name = "enemy_id"
  }

  object patch extends StringColumn(this) with PartitionKey[String]
  object tier extends IntColumn(this) with PartitionKey[Int]
  object region extends IntColumn(this) with PartitionKey[Int]
  object role extends IntColumn(this) with PartitionKey[Int]

  object matchSum extends BlobColumn(this)

  override def fromRow(r: Row): MatchSum = {
    val input = CodedInputStream.newInstance(matchSum(r))
    MatchSum.parseFrom(input)
  }
}

abstract class ConcreteMatchSumsModel extends MatchSumsModel with RootConnector {

  override val tableName = "match_sums"

  def get(filters: MatchFilters): Future[Option[MatchSum]] = {
    select
      .where(_.championId eqs filters.championId)
      .and(_.enemyId eqs filters.enemyId)
      .and(_.patch eqs filters.patch)
      .and(_.tier eqs filters.tier)
      .and(_.region eqs filters.region.value)
      .and(_.role eqs filters.role.value)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()
  }

  def sum(filters: Set[MatchFilters]): Future[MatchSum] = {
    val futures = Future.sequence(filters.map(get(_)))
    futures.map { (list) =>
      list.foldLeft(MatchSum()) {
        case (acc, v) => v match {
          case Some(x) => acc + x
          case None => acc
        }
      }
    }
  }

}
