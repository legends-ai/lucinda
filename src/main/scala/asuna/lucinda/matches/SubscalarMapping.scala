package asuna.lucinda.matches

import asuna.proto.league.MatchSum.{ Collections => SC }
import asuna.proto.league.lucinda.MatchQuotient.{ Collections => QC }

/**
  * A mapping of subscalar from SumT to QuotT.
  */
trait SubscalarMapping[SumT, QuotT] extends SubscalarExtractor[SumT] {

  /**
    * Builds a quotient type from a sum and quotient subscalars.
    */
  def build(sum: SumT, quotSub: Option[QC.Subscalars]): QuotT

}

/**
  * Allows extracting a subscalars from a sum type.
  */
trait SubscalarExtractor[SumT] {

  /**
    * Extracts the subscalars from a sum.
    */
  def extract(sum: SumT): Option[SC.Subscalars]

}

object SubscalarMapping {

  implicit object MasterySetMapping extends SubscalarMapping[SC.MasterySet, QC.MasterySet] {

    def build(sum: SC.MasterySet, quotSub: Option[QC.Subscalars]) =
      QC.MasterySet(masteries = sum.masteries, subscalars = quotSub)

    def extract(sum: SC.MasterySet) = sum.subscalars

  }

  implicit object RuneSetMapping extends SubscalarMapping[SC.RuneSet, QC.RuneSet] {

    def build(sum: SC.RuneSet, quotSub: Option[QC.Subscalars]) =
      QC.RuneSet(runes = sum.runes, subscalars = quotSub)

    def extract(sum: SC.RuneSet) = sum.subscalars

  }

  implicit object KeystoneMapping extends SubscalarMapping[SC.Keystone, QC.Keystone] {

    def build(sum: SC.Keystone, quotSub: Option[QC.Subscalars]) =
      QC.Keystone(keystone = sum.keystone, subscalars = quotSub)

    def extract(sum: SC.Keystone) = sum.subscalars

  }

  implicit object SummonerSetMapping extends SubscalarMapping[SC.SummonerSet, QC.SummonerSet] {

    def build(sum: SC.SummonerSet, quotSub: Option[QC.Subscalars]) =
      QC.SummonerSet(spell1 = sum.spell1, spell2 = sum.spell2, subscalars = quotSub)

    def extract(sum: SC.SummonerSet) = sum.subscalars

  }

  implicit object TrinketMapping extends SubscalarMapping[SC.Trinket, QC.Trinket] {

    def build(sum: SC.Trinket, quotSub: Option[QC.Subscalars]) =
      QC.Trinket(trinket = sum.trinket, subscalars = quotSub)

    def extract(sum: SC.Trinket) = sum.subscalars

  }

  implicit object SkillOrderMapping extends SubscalarMapping[SC.SkillOrder, QC.SkillOrder] {

    def build(sum: SC.SkillOrder, quotSub: Option[QC.Subscalars]) =
      QC.SkillOrder(skillOrder = sum.skillOrder, subscalars = quotSub)

    def extract(sum: SC.SkillOrder) = sum.subscalars

  }

  implicit object ItemListMapping extends SubscalarMapping[SC.ItemList, QC.ItemList] {

    def build(sum: SC.ItemList, quotSub: Option[QC.Subscalars]) =
      QC.ItemList(items = sum.items, subscalars = quotSub)

    def extract(sum: SC.ItemList) = sum.subscalars

  }

}
