package asuna.lucinda.matches

import asuna.proto.league.lucinda.{ MatchQuotient, Statistics }


/**
  * Applies min play rate to every element in each collection.
  */
object MinPickRateDecorator {

  def decorate(minPickRate: Double, stats: Statistics): Statistics = {
    stats.copy(
      collections = stats.collections
        .map(colls => decorateCollections(minPickRate, colls))
    )
  }

  def meetsMinPickRate(minPickRate: Double)(ss: Option[MatchQuotient.Collections.Subscalars]): Boolean = {
    ss.map(_.playRate >= minPickRate).getOrElse(false)
  }

  /**
    * Applies min play rate to everything
    */
  def decorateCollections(minPickRate: Double, colls: MatchQuotient.Collections): MatchQuotient.Collections = {
    val mpr = meetsMinPickRate(minPickRate) _
    colls.copy(
      masteries = colls.masteries.filter(s => mpr(s.subscalars)),
      runes = colls.runes.filter(s => mpr(s.subscalars)),
      keystones = colls.keystones.filter(s => mpr(s.subscalars)),
      summoners = colls.summoners.filter(s => mpr(s.subscalars)),
      startingTrinkets = colls.startingTrinkets.filter(s => mpr(s.subscalars)),
      endingTrinkets = colls.endingTrinkets.filter(s => mpr(s.subscalars)),
      skillOrders = colls.skillOrders.filter(s => mpr(s.subscalars)),
      starterItems = colls.starterItems.filter(s => mpr(s.subscalars)),
      coreBuilds = colls.coreBuilds.filter(s => mpr(s.subscalars))
    )
  }

}
