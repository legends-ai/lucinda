package asuna.lucinda.matches

import asuna.proto.league.lucinda.{ MatchQuotient, Statistics }
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

  implicit class MPRAndTopK[T](seq: Seq[T]) {
    def mprAndTopK(mpr: Double, k: Int)(implicit qse: QuotSubscalarsExtractor[T]): Seq[T] = {
      seq
        .sortBy(x => qse.subscalars(x).map(_.playRate).orEmpty)
        .reverse
        .take(k)
        .filter { el =>
          qse.subscalars(el).map(_.playRate).orEmpty >= mpr
        }
    }
  }

}


/**
  * Applies min pick rate and top k to every element in each collection.
  */
object MinPickRateDecorator {
  import QuotSubscalarsExtractor._

  def decorate(minPickRate: Double, topK: Int, stats: Statistics): Statistics = {
    stats.copy(
      collections = stats.collections
        .map(colls => decorateCollections(minPickRate, topK, colls))
    )
  }

  /**
    * Applies min play rate to everything
    */
  def decorateCollections(minPickRate: Double, topK: Int, colls: MatchQuotient.Collections): MatchQuotient.Collections = {
    colls.copy(
      masteries = colls.masteries.mprAndTopK(minPickRate, topK),
      runes = colls.runes.mprAndTopK(minPickRate, topK),
      keystones = colls.keystones.mprAndTopK(minPickRate, topK),
      summoners = colls.summoners.mprAndTopK(minPickRate, topK),
      startingTrinkets = colls.startingTrinkets.mprAndTopK(minPickRate, topK),
      endingTrinkets = colls.endingTrinkets.mprAndTopK(minPickRate, topK),
      skillOrders = colls.skillOrders.mprAndTopK(minPickRate, topK),
      starterItems = colls.starterItems.mprAndTopK(minPickRate, topK),
      coreBuilds = colls.coreBuilds.mprAndTopK(minPickRate, topK)
    )
  }

}