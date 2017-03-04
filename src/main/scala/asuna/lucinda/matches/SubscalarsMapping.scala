package asuna.lucinda.matches

import asuna.proto.league.MatchSum.Collections.{ Subscalars => SSubscalars }
import asuna.proto.league.lucinda.MatchQuotient.Collections.{ Subscalars => QSubscalars }
import asuna.proto.league.MatchSum.{ Collections => SC }
import asuna.proto.league.lucinda.MatchQuotient.{ Collections => QC }

/**
  * A mapping of statistics from SumT to QuotT.
  */
trait SubscalarsMapping[SumT, QuotT] extends SubscalarsExtractor[SumT] {

  /**
    * Builds a quotient type from a sum and quotient subscalars.
    */
  def build(sum: SumT, quotSub: Option[QSubscalars]): QuotT

}

/**
  * Allows extracting a subscalars from a sum type.
  */
trait SubscalarsExtractor[SumT] {

  /**
    * Extracts the subscalars from a sum.
    */
  def extract(sum: SumT): Option[SSubscalars]

}

object SubscalarsMapping {

  implicit object MasterySetMapping extends SubscalarsMapping[SC.MasterySet, QC.MasterySet] {

    def build(sum: SC.MasterySet, quotSub: Option[QSubscalars]) =
      QC.MasterySet(masteries = sum.masteries, subscalars = quotSub)

    def extract(sum: SC.MasterySet) = sum.subscalars

  }

  implicit object RuneSetMapping extends SubscalarsMapping[SC.RuneSet, QC.RuneSet] {

    def build(sum: SC.RuneSet, quotSub: Option[QSubscalars]) =
      QC.RuneSet(runes = sum.runes, subscalars = quotSub)

    def extract(sum: SC.RuneSet) = sum.subscalars

  }

  implicit object KeystoneMapping extends SubscalarsMapping[SC.Keystone, QC.Keystone] {

    def build(sum: SC.Keystone, quotSub: Option[QSubscalars]) =
      QC.Keystone(keystone = sum.keystone, subscalars = quotSub)

    def extract(sum: SC.Keystone) = sum.subscalars

  }

  implicit object SummonerSetMapping extends SubscalarsMapping[SC.SummonerSet, QC.SummonerSet] {

    def build(sum: SC.SummonerSet, quotSub: Option[QSubscalars]) =
      QC.SummonerSet(spell1 = sum.spell1, spell2 = sum.spell2, subscalars = quotSub)

    def extract(sum: SC.SummonerSet) = sum.subscalars

  }

  implicit object TrinketMapping extends SubscalarsMapping[SC.Trinket, QC.Trinket] {

    def build(sum: SC.Trinket, quotSub: Option[QSubscalars]) =
      QC.Trinket(trinket = sum.trinket, subscalars = quotSub)

    def extract(sum: SC.Trinket) = sum.subscalars

  }

  implicit object SkillOrderMapping extends SubscalarsMapping[SC.SkillOrder, QC.SkillOrder] {

    def build(sum: SC.SkillOrder, quotSub: Option[QSubscalars]) =
      QC.SkillOrder(skillOrder = sum.skillOrder, subscalars = quotSub)

    def extract(sum: SC.SkillOrder) = sum.subscalars

  }

  implicit object ItemListMapping extends SubscalarsMapping[SC.ItemList, QC.ItemList] {

    def build(sum: SC.ItemList, quotSub: Option[QSubscalars]) =
      QC.ItemList(items = sum.items, subscalars = quotSub)

    def extract(sum: SC.ItemList) = sum.subscalars

  }

}
