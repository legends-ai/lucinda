package asuna.lucinda.models

import cats.implicits._
import cats.kernel.Monoid
import cats.kernel.instances.OptionMonoid
import com.websudos.phantom.dsl._
import com.google.protobuf.CodedInputStream
import asuna.proto.match_sum.MatchSum
import asuna.proto.match_filters.MatchFilters
import scala.concurrent.Future
import asuna.common.legends.MatchSumHelpers._

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
  object queue extends IntColumn(this) with PartitionKey[Int]

  object matchSum extends BlobColumn(this) {
    override def name = "match_sum"
  }

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
      .and(_.queue eqs filters.queue.value)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()
  }

  def sum(filters: Set[MatchFilters]): Future[MatchSum] = {
    // Get a list of filters
    filters.toList
      // Get the MatchSum of each filter
      .map(get)
      // Aggregate using the MatchSum monoid
      .combineAll.map(_.orEmpty)
  }

}
