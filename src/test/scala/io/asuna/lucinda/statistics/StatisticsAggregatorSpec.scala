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
      val stats = StatisticsAggregator.makeStatistics(role, inSums)
      val sums = stats.sums
      val scalars = sums.flatMap(_.scalars).get

      // Ensure all maps are the correct size
      Seq[Map[Int, Long]](
        scalars.wins,
        scalars.goldEarned,
        scalars.kills,
        scalars.deaths,
        scalars.assists,
        scalars.damageDealt,
        scalars.damageTaken,
        scalars.minionsKilled,
        scalars.teamJungleMinionsKilled,
        scalars.enemyJungleMinionsKilled,
        scalars.structureDamage,
        scalars.killingSpree,
        scalars.wardsBought,
        scalars.wardsPlaced,
        scalars.wardsKilled,
        scalars.crowdControl,
        scalars.firstBlood,
        scalars.firstBloodAssist,
        scalars.doublekills,
        scalars.triplekills,
        scalars.quadrakills,
        scalars.pentakills,
        scalars.physicalDamage,
        scalars.magicDamage,
        scalars.trueDamage
      ).foreach { map =>
        map.size should be (inSums.size)
      }

      val deltas = sums.flatMap(_.deltas).get
      Seq[Option[Sums.Deltas.Delta]](
        deltas.csDiff,
        deltas.xpDiff,
        deltas.damageTakenDiff,
        deltas.xpPerMin,
        deltas.goldPerMin,
        deltas.towersPerMin,
        deltas.wardsPlaced,
        deltas.damageTaken
      ).foreach {
        deltaOpt => deltaOpt match {
          case Some(delta) => Seq[Map[Int, Double]](
            delta.zeroToTen,
            delta.tenToTwenty,
            delta.twentyToThirty,
            delta.thirtyToEnd
          ).foreach { map =>
            map.size should be (inSums.size)
          }
          case None => None
        }
      }

      val dds = sums.flatMap(_.durationDistributions).get
      Seq[Map[Int, Long]](
        dds.zeroToTen,
        dds.tenToTwenty,
        dds.twentyToThirty,
        dds.thirtyToEnd
      ).foreach { map =>
        map.size should be (inSums.size)
      }

      val subscalars = sums.flatMap(_.subscalars).get
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

    // TODO(igm): more rigorous proof of correctness
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
