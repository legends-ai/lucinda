package asuna.lucinda.matches

import asuna.proto.league.lucinda._
import asuna.proto.league.lucinda.rpc._
import MatchQuotient.Collections._
import cats.implicits._

trait QuotSubscalarsExtractor[QuotT] {
  def subscalars(e: QuotT): Option[Subscalars]
}

object QuotSubscalarsExtractor {

  def fromFn[QuotT](f: QuotT => Option[Subscalars]): QuotSubscalarsExtractor[QuotT] =
    new QuotSubscalarsExtractor[QuotT] {
      override def subscalars(e: QuotT): Option[Subscalars] = f(e)
    }

  implicit val masterySetQSE: QuotSubscalarsExtractor[MasterySet] = fromFn(_.subscalars)
  implicit val runeSetQSE: QuotSubscalarsExtractor[RuneSet] = fromFn(_.subscalars)
  implicit val keystoneQSE: QuotSubscalarsExtractor[Keystone] = fromFn(_.subscalars)
  implicit val summonerSetQSE: QuotSubscalarsExtractor[SummonerSet] = fromFn(_.subscalars)
  implicit val trinketQSE: QuotSubscalarsExtractor[Trinket] = fromFn(_.subscalars)
  implicit val skillOrderQSE: QuotSubscalarsExtractor[SkillOrder] = fromFn(_.subscalars)
  implicit val itemListQSE: QuotSubscalarsExtractor[ItemList] = fromFn(_.subscalars)
  implicit def elQSE[T]: QuotSubscalarsExtractor[(T, Subscalars)] = fromFn(_._2.some)

}

/**
  * Allows applying constraints to something.
  */
trait Constrainer[T] {
  def constrain(in: T, constraints: Constraints): T
}

object Constrainer {
  import QuotSubscalarsExtractor._

  implicit class Ops[T](in: T) {
    def constrain(constraints: Constraints)(implicit constrainer: Constrainer[T]): T = {
      constrainer.constrain(in, constraints)
    }
  }

  def instance[T](fn: (T, Constraints) => T): Constrainer[T] = new Constrainer[T] {
    def constrain(in: T, constraints: Constraints): T = fn(in, constraints)
  }

  def id[T]: Constrainer[T] = instance((x, _) => x)

  def apply[T](implicit constrainer: Constrainer[T]): Constrainer[T] = constrainer

  implicit def qseConstrainer[T](implicit qse: QuotSubscalarsExtractor[T]): Constrainer[Seq[T]] =
    instance { (seq, constraints) =>
      seq
        .sortBy(x => qse.subscalars(x).map(_.playRate).orEmpty)
        .reverse
        .take(constraints.limit)
        .filter { el =>
          qse.subscalars(el).map(_.playRate).orEmpty >= constraints.minPickRate
        }
    }

  // roles should only constrain the role stats
  implicit val rolesConstrainer: Constrainer[Statistics.Roles] = instance { (roles, constraints) =>
    roles.copy(
      roleStats = roles.roleStats.filter(_.pickRate >= constraints.minPickRate),
      )
  }

  implicit val collectionsConstrainer: Constrainer[MatchQuotient.Collections] = instance { (colls, constraints) =>
    colls.copy(
      masteries = colls.masteries.constrain(constraints),
      runes = colls.runes.constrain(constraints),
      keystones = colls.keystones.constrain(constraints),
      summoners = colls.summoners.constrain(constraints),
      startingTrinkets = colls.startingTrinkets.constrain(constraints),
      endingTrinkets = colls.endingTrinkets.constrain(constraints),
      allies = colls.allies.constrain(constraints.copy(limit = 10000)),
      enemies = colls.enemies.constrain(constraints.copy(limit = 10000)),
      skillOrders = colls.skillOrders.constrain(constraints),
      starterItems = colls.starterItems.constrain(constraints),
      coreBuilds = colls.coreBuilds.constrain(constraints),
    )
  }

  implicit val ocsConstrainer: Constrainer[Seq[OtherChampionStats]] = instance { (seq, constraints) =>
    // TODO(igm): make a separate constraint for this
    val newCons = constraints.copy(limit = 10000)
    seq.map { ocs =>
      ocs.copy(stats = Constrainer[Seq[(Int, Subscalars)]].constrain(ocs.stats.toSeq, newCons).toMap)
    }
  }

  implicit val statsConstrainer: Constrainer[Statistics] = instance { (stats, constraints) =>
    stats.copy(
      roles = stats.roles.map(_.constrain(constraints)),
      collections = stats.collections.map(_.constrain(constraints)),
    )
  }

}

/**
  * Applies min pick rate and top k to every element in each collection.
  */
object MinPickRateDecorator {
  import Constrainer._

  def decorate(constraints: Constraints)(stats: Statistics): Statistics = {
    val newCons = if (constraints.limit === 0) {
      constraints.copy(limit = 10)
    } else {
      constraints
    }
    Constrainer[Statistics].constrain(stats, newCons)
  }

}
