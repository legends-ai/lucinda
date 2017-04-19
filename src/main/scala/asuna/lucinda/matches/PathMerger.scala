package asuna.lucinda.matches

import asuna.proto.league.Ability
import asuna.proto.league.MatchSum.Collections.ItemList
import asuna.proto.league.MatchSum.Collections.SkillOrder
import asuna.proto.league.MatchSum.Collections.Subscalars
import cats.PartialOrder
import asuna.common.legends.MatchSumHelpers._
import cats.Eq
import cats.implicits._


/**
  * Allows merging of path lists.
  */
trait PathMerger[T] {

  val order: PartialOrder[T]

  /**
    * Returns true if the path's stats should be included in the result.
    * Elements of any path of which this path is a subset will have these stats added.
    * This should be a superset of path included stats.
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
    val resultPaths = statsPaths.filter(x => isPathIncluded(x, in))

    resultPaths.map { path =>
      val lists = statsPaths.filter { path2 =>
        order.tryCompare(path, path2) match {
          // We will make sure path contains all of path 2.
          case Some(res) => res > 0
          // If incomparable, not useful.
          case None => false
        }
      }
      // combine subscalars
      val subscalars = (x.extract(path) :: lists.map(x.extract).toList).combineAll
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
  def sequencePartialOrder[E]: PartialOrder[Seq[E]] = PartialOrder.from { (a, b) =>
    val alen = a.size
    val blen = b.size
    if (alen == blen) {
      if (a == b) {
        0
      } else {
        Double.NaN
      }
    } else if (alen < blen) {
      if (b.indexOfSlice(a) == 0) {
        -1
      } else {
        Double.NaN
      }
    } else {
      if (a.indexOfSlice(b) == 0) {
        1
      } else {
        Double.NaN
      }
    }
  }

  val sightstone = 2049

  implicit object skillOrderMerger extends PathMerger[SkillOrder] {

    implicit val abilityEq = Eq.fromUniversalEquals[Ability]

    val evolutions = Set(Ability.Q_EV, Ability.W_EV, Ability.E_EV, Ability.R_EV)

    override def isPathIncluded(in: SkillOrder, others: Seq[SkillOrder]): Boolean = {
      if (in.skillOrder.exists(evolutions)) {
        in.skillOrder.size == 21 && (in.skillOrder(17) =!= Ability.R)
      } else {
        in.skillOrder.size == 18 && (in.skillOrder(15) =!= Ability.R)
      }
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

    override def isPathIncluded(in: ItemList, others: Seq[ItemList]): Boolean = {
      // TODO(igm): corrupting pot is core on singed. should it be included here?
      if (in.items.contains(sightstone)) {
        in.items.size <= 7
      } else {
        in.items.size <= 6
      }
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
