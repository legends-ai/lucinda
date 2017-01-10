package asuna.lucinda.statistics

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks

import asuna.proto.match_sum.MatchSum
import asuna.proto.lucinda.LucindaData.ChampionStatistics.Sums

class SumCombinerSpec extends PropSpec with PropertyChecks with Matchers with MatchSumGeneratorHelper {

  property("generated sums are grouped by champion id") {

    forAll { (inSums: Map[Int, MatchSum]) =>
      // Run the sum combiner!
      val sums = SumCombiner.combineSums(inSums)

      // We will be testing all properties, so let's grab them.
      // These all should exist in all instances, even if the inSums data is invalid.
      val scalars = sums.scalars.get
      val deltas = sums.deltas.get
      val dds = sums.durationDistributions.get
      val subscalars = sums.subscalars.get

      scalars.plays.foreach { case (_, plays) =>
        plays should be >= (0L)
      }

      // The number of champs that are found for this role.
      // This should be constant across all maps.
      val roleChamps = scalars.plays.size

      // Let's ensure that this number is sensible. It should be <= the total champions in the map.
      roleChamps should be <= (inSums.size)

      // Let's first check the integrity of our MatchSum scalars.
      // Let's iterate over all of the properties. We will also supply a function
      // to extract the scalars.
      Seq[(Map[Int, Long], MatchSum.Scalars => Long)](
        (scalars.plays, _.plays),
        (scalars.wins, _.wins),
        (scalars.goldEarned, _.goldEarned),
        (scalars.kills, _.kills),
        (scalars.deaths, _.deaths),
        (scalars.assists, _.assists),
        (scalars.damageDealt, _.damageDealt),
        (scalars.damageTaken, _.damageTaken),
        (scalars.minionsKilled, _.minionsKilled),
        (scalars.teamJungleMinionsKilled, _.teamJungleMinionsKilled),
        (scalars.enemyJungleMinionsKilled, _.enemyJungleMinionsKilled),
        (scalars.structureDamage, _.structureDamage),
        (scalars.killingSpree, _.killingSpree),
        (scalars.wardsBought, _.wardsBought),
        (scalars.wardsPlaced, _.wardsPlaced),
        (scalars.wardsKilled, _.wardsKilled),
        (scalars.crowdControl, _.crowdControl),
        (scalars.firstBlood, _.firstBlood),
        (scalars.firstBloodAssist, _.firstBloodAssist),
        (scalars.doublekills, _.doublekills),
        (scalars.triplekills, _.triplekills),
        (scalars.quadrakills, _.quadrakills),
        (scalars.pentakills, _.pentakills),
        (scalars.physicalDamage, _.physicalDamage),
        (scalars.magicDamage, _.magicDamage),
        (scalars.trueDamage, _.trueDamage)
      ).foreach {
        case (map, scalarFn) =>
          // The size of the map should be less than or equal to the size of the input map.
          // This corresponds to number of champions that have data.
          map.size should be (roleChamps)

          // Now, we will ensure that all of our values in the map are correct.
          // Let's iterate over the key/value pairs in the map:
          map.foreach { case (key, value) =>
            // First, let's fetch the input sum that corresponds to the key (i.e. champion).
            val inSum = inSums.get(key)

            // If it's not there, the map should not contain this value.
            // This should never happen, and we will fail here.
            if (!inSum.isDefined) {
              fail("Map contains rogue value")
            }

            // If it's there, we should have the correct value.
            scalarFn(inSum.flatMap(_.scalars).get) should be (value)
          }
      }

      // Next, we will verify the integrity of the deltas object in a similar fashion.
      Seq[(Option[Sums.Deltas.Delta], MatchSum.Deltas => Option[MatchSum.Deltas.Delta])](
        (deltas.csDiff, _.csDiff),
        (deltas.xpDiff, _.xpDiff),
        (deltas.damageTakenDiff, _.damageTakenDiff),
        (deltas.xpPerMin, _.xpPerMin),
        (deltas.goldPerMin, _.goldPerMin),
        (deltas.towersPerMin, _.towersPerMin),
        (deltas.wardsPlaced, _.wardsPlaced),
        (deltas.damageTaken, _.damageTaken)
      ).foreach { case (deltaOpt, deltasFn) =>
        // Let's extract the delta from the deltas and the function to build delta from the MatchSum.

        // This should never happen.
        if (!deltaOpt.isDefined) {
          fail("Deltas missing!")
        }
        val delta = deltaOpt.get

        // We know that this delta exists (good!) so let's now build another sequence.
        // This sequence will contain extractors for Delta fields.
        Seq[(Map[Int, Double], MatchSum.Deltas.Delta => Double)](
          (delta.zeroToTen, _.zeroToTen),
          (delta.tenToTwenty, _.tenToTwenty),
          (delta.twentyToThirty, _.twentyToThirty),
          (delta.thirtyToEnd, _.thirtyToEnd)
        ).foreach { case (map, deltaFn) =>
          // Similarly, we should check that the size of each map is <= size of the input map.
          map.size should be (roleChamps)

          // Again, we will also iterate over the map in the same fashion.
          map.foreach { case (key, value) =>
            // First, let's fetch the input sum that corresponds to the key (i.e. champion).
            val inSum = inSums.get(key)

            // If it's not there, the map should not contain this value.
            // This should never happen, and we will fail here.
            if (!inSum.isDefined) {
              fail("Map of deltas contains rogue value")
            }

            // If it's there, we should have the correct value.
            val deltas = deltasFn(inSum.flatMap(_.deltas).get)
            val delta = deltaFn(deltas.get)
            delta should be (value)
          }
        }

      }

      Seq[(Map[Int, Long], MatchSum.DurationDistribution => Long)](
        (dds.zeroToTen, _.zeroToTen),
        (dds.tenToTwenty, _.tenToTwenty),
        (dds.twentyToThirty, _.twentyToThirty),
        (dds.thirtyToEnd, _.thirtyToEnd)
      ).foreach { case (map, ddFn) =>
        map.size should be (roleChamps)

          map.foreach { case (key, value) =>
            val inSum = inSums.get(key)
            if (!inSum.isDefined) {
              fail("Map of duration distributions contains rogue values")
            }

            ddFn(inSum.flatMap(_.durationDistribution).get) should be (value)
          }
      }

      Seq[Map[Int, Sums.Subscalars.Subscalar]](
        subscalars.bans,
        subscalars.allies
      ).foreach { ssMap =>
        ssMap.values.foreach { ss =>
          Seq[Map[Int, Long]](
            ss.plays,
            ss.wins
          ).foreach { map =>
            map.size should be (roleChamps)
          }
        }
      }

    }
  }
}
