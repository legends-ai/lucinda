package asuna.lucinda.matches

import org.scalatest._
import cats.implicits._

import asuna.proto.league.MatchSum.Collections.ItemList
import asuna.proto.league.MatchSum.Collections.Subscalars

class PathMergerSpec extends FlatSpec with Matchers {
  import SubscalarsMapping._
  import PathMerger._

  "Item List Path Merger" should "merge all paths" in {
    val paths = List(
      ItemList(
        items = Seq(1, 2, 3, 4, 5, 6),
        subscalars = Subscalars(
          plays = 3,
          wins = 1
        ).some
      ),
      ItemList(
        items = Seq(1, 2, 3, 4),
        subscalars = Subscalars(
          plays = 10,
          wins = 4
        ).some
      ),
      ItemList(
        items = Seq(1, 2, 3),
        subscalars = Subscalars(
          plays = 3,
          wins = 2
        ).some
      ),
      ItemList(
        items = Seq(1, 3, 4),
        subscalars = Subscalars(
          plays = 150,
          wins = 4
        ).some
      ),
    )

    val expected = List(
      ItemList(
        items = Seq(1, 2, 3, 4, 5, 6),
        subscalars = Subscalars(
          plays = 16,
          wins = 7
        ).some
      ),
      ItemList(
        items = Seq(1, 2, 3, 4),
        subscalars = Subscalars(
          plays = 13,
          wins = 6
        ).some
      ),
      ItemList(
        items = Seq(1, 2, 3),
        subscalars = Subscalars(
          plays = 3,
          wins = 2
        ).some
      ),
      ItemList(
        items = Seq(1, 3, 4),
        subscalars = Subscalars(
          plays = 150,
          wins = 4
        ).some
      ),
    )
    paths.mergePaths should be (expected)
  }

}
