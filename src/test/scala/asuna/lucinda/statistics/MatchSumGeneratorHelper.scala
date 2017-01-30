package asuna.lucinda.statistics

import asuna.proto.league.{ MatchSum, Region, Role, QueueType }
import asuna.proto.league.lucinda.ChampionStatistics
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import Arbitrary.arbitrary

/**
  * Helper for generating valid MatchSum maps.
  * WARNING: this code is a steaming pile of shit. This warrants a major refactor!
  * It's extremely confusing and not ordered well. We should separate concerns!
  */
trait MatchSumGeneratorHelper {

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
      // TODO(igm): make these maps valid. it messes up the data
      masteries <- genStrSubscalarsMap
      runes <- genStrSubscalarsMap
      keystones <- genStrSubscalarsMap
      summoners <- genStrSubscalarsMap
      skillOrders <- genStrSubscalarsMap
      durationDistribution <- genDurationDistribution
      bans <- genChampionSubscalarsMap
      allies <- genChampionSubscalarsMap
      enemies <- genChampionSubscalarsMap
      starterItems <- genStrSubscalarsMap
      buildPath <- genStrSubscalarsMap
      items <- genIntSubscalarsMap
    } yield MatchSum(
      scalars = Option(scalars),
      deltas = Option(deltas),
      masteries = masteries,
      runes = runes,
      summoners = summoners,
      skillOrders = skillOrders,
      durationDistribution = Option(durationDistribution),
      bans = bans,
      allies = allies,
      enemies = enemies,
      starterItems = starterItems,
      buildPath = buildPath,
      items = items
    )
  }

  // Builds a generator for a Map[Int, MatchSum] given a champion list
  def makeMatchSumMap(champs: Set[Int]): Gen[Map[Int, MatchSum]] = {
    for {
      sums <- Gen.containerOfN[List, MatchSum](champs.size, makeGenMatchSum(champs))
    } yield champs.zip(sums).toMap
  }

  val genMatchSumMap = for {
    champs <- Gen.containerOf[Set, Int](Gen.choose(1, 100))
    sumMap <- makeMatchSumMap(champs)
  } yield sumMap

  // Arbitrary region
  implicit lazy val arbRegion: Arbitrary[Region] = Arbitrary(Gen.oneOf(Region.values))

  // Arbitrary role
  implicit lazy val arbRole: Arbitrary[Role] = Arbitrary(Gen.oneOf(Role.values))

  // Arbitrary queue
  implicit lazy val arbQueue: Arbitrary[QueueType] = Arbitrary(Gen.oneOf(QueueType.values))

  // Arbitrary map of sums
  implicit lazy val arbSumMap: Arbitrary[Map[Int, MatchSum]] = Arbitrary(genMatchSumMap)

  // TODO(igm): generate these sums independently
  val genSums = for {
    sumMap <- genMatchSumMap
  } yield SumCombiner.combineSums(sumMap)

  implicit lazy val arbSums: Arbitrary[ChampionStatistics.Sums] = Arbitrary(genSums)

  // TODO(igm): generate these quotients independently
  val genQuotients = for {
    sums <- genSums
  } yield QuotientsGenerator.generateQuotients(sums)

  implicit lazy val arbQuotients: Arbitrary[ChampionStatistics.Quotients] = Arbitrary(genQuotients)

  val genMatchAggregatorArgs: Gen[MatchAggregatorArgs] = {
    for {
      champion <- arbitrary[Int]
      minPlayRate <- arbitrary[Double]
      otherChamps <- Gen.containerOf[Set, Int](Gen.choose(1, 100))
      patches <- Gen.containerOf[Set, String](arbitrary[String])
      role <- arbitrary[Role]
      champs = otherChamps + champion

      // Generate a ChampionStatistics object for every patch.
      // This is probably the most expensive operation here. #bigdata
      patchSums <- Gen.containerOfN[List, Map[Int, MatchSum]](patches.size, makeMatchSumMap(champs))
      patchStats = patches.zip(patchSums).map { case (patch, sumMap) =>
        (patch, StatisticsAggregator.makeStatistics(role, sumMap))
      }.toMap

      // Generate a MatchSum for every role.
      // TODO(igm): use valid data
      roleSums <- Gen.containerOfN[List, MatchSum](Role.values.size, makeGenMatchSum(champs))
      byRole = Role.values.zip(roleSums).toMap

      patchSums <- Gen.containerOfN[List, MatchSum](patches.size, makeGenMatchSum(champs))
      byPatch = patches.zip(patchSums).toMap

    } yield {
      MatchAggregatorArgs(
        champion = champion,
        minPlayRate = minPlayRate,
        patchStats = patchStats,
        byRole = byRole,
        byPatch = byPatch
      )
    }
  }

  implicit lazy val arbMatchAggregatorArgs: Arbitrary[MatchAggregatorArgs] = Arbitrary(genMatchAggregatorArgs)
}

// Below is used to test the match aggregator.
// TODO(igm): reevalutate whether this is a good testing strategy. It's messy af.
case class MatchAggregatorArgs(
  champion: Int,
  minPlayRate: Double,
  patchStats: Map[String, ChampionStatistics],
  byRole: Map[Role, MatchSum],
  byPatch: Map[String, MatchSum]
)
