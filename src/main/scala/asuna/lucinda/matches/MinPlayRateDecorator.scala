package asuna.lucinda.matches

import asuna.proto.league.lucinda.{ MatchQuotient, Statistics }


/**
  * Applies min play rate to every element in each collection.
  */
object MinPlayRateDecorator {

  def decorate(minPlayRate: Double, stats: Statistics): Statistics = {
    stats.copy(
      collections = stats.collections
        .map(colls => decorateCollections(minPlayRate, colls))
    )
  }

  def meetsMinPlayRate(minPlayRate: Double)(ss: Option[MatchQuotient.Collections.Subscalars]): Boolean = {
    ss.map(_.playRate >= minPlayRate).getOrElse(false)
  }

  /**
    * Applies min play rate to everything
    */
  def decorateCollections(minPlayRate: Double, colls: MatchQuotient.Collections): MatchQuotient.Collections = {
    val mpr = meetsMinPlayRate(minPlayRate) _
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
