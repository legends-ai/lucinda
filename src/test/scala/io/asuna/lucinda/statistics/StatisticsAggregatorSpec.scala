package io.asuna.lucinda.statistics

import org.scalatest._
import org.scalacheck._
import prop._
import scala.collection.immutable._
import io.asuna.proto.enums.Role
import io.asuna.proto.match_sum.MatchSum
import io.asuna.proto.lucinda.LucindaData.ChampionStatistics

class StatisticsAggregatorSpec extends PropSpec with PropertyChecks with Matchers {
  import Arbitrary.arbitrary

  implicit lazy val arbRole: Arbitrary[Role] = Arbitrary(Gen.oneOf(Role.values))

  val genScalars = for {
    wins <- arbitrary[Long]
    goldEarned <- arbitrary[Long]
    kills <- arbitrary[Long]
    deaths <- arbitrary[Long]
    assists <- arbitrary[Long]
    damageDealt <- arbitrary[Long]
    damageTaken <- arbitrary[Long]
    minionsKilled <- arbitrary[Long]
    teamJungleMinionsKilled <- arbitrary[Long]
    enemyJungleMinionsKilled <- arbitrary[Long]
    structureDamage <- arbitrary[Long]
    killingSpree <- arbitrary[Long]
    wardsBought <- arbitrary[Long]
    wardsPlaced <- arbitrary[Long]
    wardsKilled <- arbitrary[Long]
    crowdControl <- arbitrary[Long]
    firstBlood <- arbitrary[Long]
    firstBloodAssist <- arbitrary[Long]
    doublekills <- arbitrary[Long]
    triplekills <- arbitrary[Long]
    quadrakills <- arbitrary[Long]
    pentakills <- arbitrary[Long]
    physicalDamage <- arbitrary[Long]
    magicDamage <- arbitrary[Long]
    trueDamage <- arbitrary[Long]
  } yield MatchSum.Scalars(
    wins = wins,
    goldEarned = goldEarned,
    kills = kills,
    deaths = deaths,
    assists = assists,
    damageDealt = damageDealt,
    damageTaken = damageTaken,
    minionsKilled = minionsKilled,
    teamJungleMinionsKilled = teamJungleMinionsKilled,
    enemyJungleMinionsKilled = enemyJungleMinionsKilled,
    structureDamage = structureDamage,
    killingSpree = killingSpree,
    wardsBought = wardsBought,
    wardsPlaced = wardsPlaced,
    wardsKilled = wardsKilled,
    crowdControl = crowdControl,
    firstBlood = firstBlood,
    firstBloodAssist = firstBloodAssist,
    doublekills = doublekills,
    triplekills = triplekills,
    quadrakills = quadrakills,
    pentakills = pentakills,
    physicalDamage = physicalDamage,
    magicDamage = magicDamage,
    trueDamage = trueDamage
  )

  val genDelta = for {
    zeroToTen <- arbitrary[Double]
    tenToTwenty <- arbitrary[Double]
    twentyToThirty <- arbitrary[Double]
    thirtyToEnd <- arbitrary[Double]
  } yield MatchSum.Deltas.Delta(
    zeroToTen = zeroToTen,
    tenToTwenty = tenToTwenty,
    twentyToThirty = twentyToThirty,
    thirtyToEnd = thirtyToEnd
  )

  val genDeltas = for {
    csDiff <- genDelta
    xpDiff <- genDelta
    damageTakenDiff <- genDelta
    xpPerMin <- genDelta
    goldPerMin <- genDelta
    towersPerMin <- genDelta
    wardsPlaced <- genDelta
    damageTaken <- genDelta
  } yield MatchSum.Deltas(
    csDiff = Option(csDiff),
    xpDiff = Option(xpDiff),
    damageTakenDiff = Option(damageTakenDiff),
    xpPerMin = Option(xpPerMin),
    goldPerMin = Option(goldPerMin),
    towersPerMin = Option(towersPerMin),
    wardsPlaced = Option(wardsPlaced),
    damageTaken = Option(damageTaken)
  )

  val genSubscalars = for {
    plays <- arbitrary[Long]
    wins <- arbitrary[Long]
  } yield MatchSum.Subscalars(
    plays = plays,
    wins = wins
  )

  val genStrSubscalarsMap = for {
    keys <- Gen.containerOf[List, String](Arbitrary.arbitrary[String])
    values <- Gen.containerOfN[List, MatchSum.Subscalars](keys.size, genSubscalars)
  } yield keys.zip(values).toMap

  val genIntSubscalarsMap = for {
    keys <- Gen.containerOf[List, Int](Arbitrary.arbitrary[Int])
    values <- Gen.containerOfN[List, MatchSum.Subscalars](keys.size, genSubscalars)
  } yield keys.zip(values).toMap

  val genDurationDistribution = for {
    zeroToTen <- arbitrary[Long]
    tenToTwenty <- arbitrary[Long]
    twentyToThirty <- arbitrary[Long]
    thirtyToEnd <- arbitrary[Long]
  } yield MatchSum.DurationDistribution(
    zeroToTen = zeroToTen,
    tenToTwenty = tenToTwenty,
    twentyToThirty = twentyToThirty,
    thirtyToEnd = thirtyToEnd
  )

  def makeGenChampionSubscalarsMap(champions: Set[Int]) = for {
    subscalars <- Gen.containerOfN[List, MatchSum.Subscalars](champions.size, genSubscalars)
  } yield champions.zip(subscalars).toMap

  def makeGenMatchSum(champions: Set[Int]) = {
    val genChampionSubscalarsMap = makeGenChampionSubscalarsMap(champions)
    for {
      scalars <- genScalars
      deltas <- genDeltas
      masteries <- genStrSubscalarsMap
      runes <- genStrSubscalarsMap
      keystones <- genStrSubscalarsMap
      summoners <- genStrSubscalarsMap
      trinkets <- genIntSubscalarsMap
      skillOrders <- genStrSubscalarsMap
      durationDistribution <- genDurationDistribution
      bans <- genChampionSubscalarsMap
      allies <- genChampionSubscalarsMap
      enemies <- genChampionSubscalarsMap
      starterItems <- genStrSubscalarsMap
      buildPath <- genStrSubscalarsMap
    } yield MatchSum(
      scalars = Option(scalars),
      deltas = Option(deltas),
      masteries = masteries,
      runes = runes,
      keystones = keystones,
      summoners = summoners,
      trinkets = trinkets,
      skillOrders = skillOrders,
      durationDistribution = Option(durationDistribution),
      bans = bans,
      allies = allies,
      enemies = enemies,
      starterItems = starterItems,
      buildPath = buildPath
    )
  }

  val genMatchSumMap = for {
    champs <- Gen.containerOf[Set, Int](Gen.choose(1, 100))
    sums <- Gen.containerOfN[List, MatchSum](champs.size, makeGenMatchSum(champs))
  } yield champs.zip(sums).toMap

  implicit lazy val arbSumMap: Arbitrary[Map[Int, MatchSum]] = Arbitrary(genMatchSumMap)

  property("preservation of role") {
    forAll { (role: Role) =>
      val stats = StatisticsAggregator.makeStatistics(role, Map())
      stats.role should be (role)
    }
  }

  property("sums make sense") {
    import ChampionStatistics.Sums

    forAll { (role: Role, inSums: Map[Int, MatchSum]) =>
      // Run the stats aggregator!
      val stats = StatisticsAggregator.makeStatistics(role, inSums)

      // First, we will extract the sums object.
      val sums = stats.sums

      // We will be testing all properties, so let's grab them.
      // These all should exist in all instances, even if the inSums data is invalid.
      val scalars = sums.flatMap(_.scalars).get
      val deltas = sums.flatMap(_.deltas).get
      val dds = sums.flatMap(_.durationDistributions).get
      val subscalars = sums.flatMap(_.subscalars).get

      // Let's first check the integrity of our MatchSum scalars.
      // Let's iterate over all of the properties. We will also supply a function
      // to extract the scalars.
      Seq[(Map[Int, Long], MatchSum.Scalars => Long)](
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
          // The size of the map should be equivalent to the size of the input map.
          // This corresponds to number of champions.
          map.size should be (inSums.size)

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
          // Similarly, we should check that the size of each map is equivalent to the size of the input map.
          map.size should be (inSums.size)

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
        map.size should be (inSums.size)

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
            map.size should be (inSums.size)
          }
        }
      }

    }
  }

  property("quotients make sense") {
    forAll { (role: Role, inSums: Map[Int, MatchSum]) =>
      val stats = StatisticsAggregator.makeStatistics(role, inSums)
      val scalars = stats.quotients.flatMap(_.scalars).get
    }
  }

  property("results make sense") {
    forAll { (role: Role, inSums: Map[Int, MatchSum]) =>
      val stats = StatisticsAggregator.makeStatistics(role, inSums)
      val scalars = stats.results.flatMap(_.scalars).get
    }
  }

}
