package io.asuna.lucinda.statistics

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary

import io.asuna.proto.enums.Role
import io.asuna.proto.match_sum.MatchSum

/**
  * Helper for generating valid MatchSum maps.
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

  // Arbitrary role
  implicit lazy val arbRole: Arbitrary[Role] = Arbitrary(Gen.oneOf(Role.values))

  // Arbitrary map of sums
  implicit lazy val arbSumMap: Arbitrary[Map[Int, MatchSum]] = Arbitrary(genMatchSumMap)
}
