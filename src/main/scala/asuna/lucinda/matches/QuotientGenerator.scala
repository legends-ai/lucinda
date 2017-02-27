package asuna.lucinda.matches

import asuna.common.legends.MatchSumHelpers._
import asuna.proto.league.MatchSum
import asuna.proto.league.lucinda.MatchQuotient
import MatchQuotient._
import MatchQuotient.Collections._
import cats.implicits._

object QuotientGenerator {

  // Items that could be core items.
  val coreItems: Set[Int] = Set(
    // TODO(pradyuman)
  )

  def generate(sum: MatchSum): MatchQuotient = {
    import Divisor._
    sum.quotient
  }

  /**
    * Merges skill orders lesser than 18 in length.
    */
  def mergeSkillOrders(skillOrders: Seq[MatchSum.Collections.SkillOrder]): Seq[MatchSum.Collections.SkillOrder] = {
    // TODO(igm): this is pretty inefficient. we can use a trie for slightly faster calculations.
    // Verify if this pretty code is worth it.

    // find all matching prefix
    skillOrders.filter(_.skillOrder.size == 18).map { order =>
      val so = order.skillOrder
      val subscalars = skillOrders
      // check if skill orders are the same
        .filter(k => so.take(k.skillOrder.size) == k.skillOrder)
      // combine
        .toList.map(_.subscalars).combineAll
      order.copy(subscalars = subscalars)
    }
  }

  /**
    * Merges item lists that share a common prefix.
    */
  def mergeItemLists(itemLists: Seq[MatchSum.Collections.ItemList]): Seq[MatchSum.Collections.ItemList] = {
    // TODO(igm): this is pretty inefficient -- n^2. we can use a trie for better runtime.
    // Verify if this pretty code is worth it.

    // Find item lists that are leaves
    val fullItemLists = itemLists.filterNot { itemList =>
      itemLists
        // find item lists that are longer
        .filter(_.items.size > itemList.items.size)
        // Check for prefix equaling the item list
        .find(_.items.take(itemList.items.size) == itemList.items)
        // Filter this out if this is non empty
        .isDefined
    }

    // find all matching prefix
    fullItemLists.map { itemList =>
      val lists = itemLists
        // find comparable
        .filter(_.items.size < itemList.items.size)
        // check for same prefix
        .filter(x => x.items == itemList.items.take(x.items.size))
      // combine subscalars
      val subscalarList = itemList.subscalars :: lists.map(_.subscalars).toList
      itemList.copy(subscalars = subscalarList.combineAll)
    }
  }

}
