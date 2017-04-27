package asuna.lucinda.matches

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.Ability
import asuna.proto.league.MatchSum.Collections.ItemList
import asuna.proto.league.MatchSum.Collections.SkillOrder
import asuna.proto.league.MatchSum.Collections.Subscalars
import cats.{ Eq, PartialOrder }
import cats.kernel.Comparison._
import cats.implicits._

/**
  * Allows merging of path lists.
  */
trait PathMerger[T] {

  val order: PartialOrder[T]

  /**
    * Returns true if the path's stats should be included in the result.
    * Elements of any path of which this path is a subset will have these stats added.
    */
  def isStatsIncluded(in: T): Boolean = true

  /**
    * Returns true if the path should be included in the result.
    */
  def isPathIncluded(in: T, others: Seq[T]): Boolean = {
    // Check if a longer, comparable path exists
    others.exists(el => order.gteqv(el, in))
  }

  /**
    * Gives the path a new Subscalars.
    */
  def rebuild(path: T, ss: Option[Subscalars]): T

  def merge(in: Seq[T])(implicit x: SubscalarsExtractor[T]): Seq[T] = {
    // TODO(igm): this is pretty inefficient -- n^s. We can use a trie for better runtime.
    // find all matching prefix
    val statsPaths = in.filter(isStatsIncluded)
    val resultPaths = in.filter(isPathIncluded(_, in))

    // group by path and combine all scalars
    resultPaths.map { path =>
      val lists = statsPaths.filter { path2 =>
        order.tryCompare(path, path2) match {
          // We will make sure path contains all of path 2.
          case Some(res) => res >= 0
          // If incomparable, not useful.
          case None => false
        }
      }
      // combine subscalars
      val subscalars = lists.map(x.extract).toList.combineAll
      rebuild(path, subscalars)
    }
  }

}

object PathMerger {

  /**
    * Partial order on sequence.
    * Kind of like subset but cares about position and order.
    * @param E -- path element
    */
  def sequencePartialOrder[E: Eq]: PartialOrder[Seq[E]] = PartialOrder.from { (a, b) =>
    a.size comparison b.size match {
      case LessThan => if (b.indexOfSlice(a) === 0) -1 else Double.NaN
      case GreaterThan => if (a.indexOfSlice(b) === 0) 1 else Double.NaN
      case EqualTo => if (a.toVector === b.toVector) 0 else Double.NaN
    }
  }

  val sightstone = 2049

  implicit object skillOrderMerger extends PathMerger[SkillOrder] {

    implicit val abilityEq = Eq.fromUniversalEquals[Ability]

    val evolutions = Set(Ability.Q_EV, Ability.W_EV, Ability.E_EV, Ability.R_EV)

    override def isStatsIncluded(in: SkillOrder): Boolean = {
      in.skillOrder.size <= 15
    }

    override def isPathIncluded(in: SkillOrder, others: Seq[SkillOrder]): Boolean = {
      in.skillOrder.size === 18
    }

    override val order: PartialOrder[SkillOrder] =
      sequencePartialOrder[Ability].on[SkillOrder](_.skillOrder)

    def rebuild(path: SkillOrder, ss: Option[Subscalars]): SkillOrder = {
      path.copy(subscalars = ss)
    }

  }

  implicit object itemListMerger extends PathMerger[ItemList] {

    override val order: PartialOrder[ItemList] =
      sequencePartialOrder[Int].on[ItemList](_.items)

    override def isStatsIncluded(in: ItemList): Boolean = {
      in.items.size > 2
    }

    override def isPathIncluded(in: ItemList, others: Seq[ItemList]): Boolean = {
      // TODO(igm): corrupting pot is core on singed. should it be included here?
      if (in.items.contains(sightstone))
        in.items.size <= 7
      else
        in.items.size <= 6
    }

    def rebuild(path: ItemList, ss: Option[Subscalars]): ItemList = {
      path.copy(subscalars = ss)
    }

  }

  implicit class PathMergeableSeq[T](seq: Seq[T]) {
    def mergePaths(implicit merger: PathMerger[T], x: SubscalarsExtractor[T]): Seq[T] = {
      merger.merge(seq)
    }
  }

}
