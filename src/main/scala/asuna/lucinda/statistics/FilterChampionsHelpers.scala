package asuna.lucinda.statistics

import asuna.proto.league.lucinda.Statistic
import asuna.proto.league.lucinda.AllChampionStatistics.Results
import shapeless.{ Generic, HList, ::, HNil, Lazy }

trait FilterChampions[A] {
  def filter(in: A, champions: Set[Int]): A
}

object FilterChampions {

  def apply[A](implicit filter: FilterChampions[A]) = filter

  def pure[A](f: (A, Set[Int]) => A) = new FilterChampions[A] {
    def filter(in: A, champions: Set[Int]) = f(in, champions)
  }

  implicit val statisticFilter =
    pure[Map[Int, Statistic]]((in, c) => in.filterKeys(c))

  implicit val dragonsDeriver =
    pure[Seq[Results.Scalars.DragonStat]] { (dragons, c) =>
      dragons map { stat =>
        Results.Scalars.DragonStat(
          dragon = stat.dragon,
          // TODO(p): Figure out if there is a way for shapeless to derive this implicitly
          value = statisticFilter.filter(stat.value, c)
        )
      }
    }

  implicit def optionFilter[A](implicit filter: FilterChampions[A]) =
    pure[Option[A]]((opt, c) => opt.map(in => filter.filter(in, c)))

  implicit val hnilDeriver = pure[HNil]((_, _) => HNil)

  implicit def hlistFilter[H, T <: HList](
    implicit
    hFilter: Lazy[FilterChampions[H]],
    tFilter: Lazy[FilterChampions[T]]
  ) = pure[H :: T] { case (head :: tail, champions) =>
      hFilter.value.filter(head, champions) :: tFilter.value.filter(tail, champions)
  }

  implicit def genericFilter[A, R](
    implicit
    gen: Generic.Aux[A, R],
    filter: FilterChampions[R]
  ) = pure[A]((in, c) => gen.from(filter.filter(gen.to(in), c)))

}

object FilterChampionsHelpers {

  implicit class ScalarsFilterChampions(scalars: Results.Scalars) {

    def filterChampions(champs: Set[Int]): Results.Scalars =
      FilterChampions[Results.Scalars].filter(scalars, champs)

  }

  implicit class DeltaFilterChampions(delta: Results.Deltas.Delta) {

    def filterChampions(champs: Set[Int]): Results.Deltas.Delta =
      FilterChampions[Results.Deltas.Delta].filter(delta, champs)

  }

  implicit class DeltasFilterChampions(deltas: Results.Deltas) {

    def filterChampions(champs: Set[Int]): Results.Deltas =
      FilterChampions[Results.Deltas].filter(deltas, champs)

  }

  implicit class DerivativesFilterChampions(derivatives: Results.Derivatives) {

    def filterChampions(champs: Set[Int]): Results.Derivatives =
      FilterChampions[Results.Derivatives].filter(derivatives, champs)

  }

  implicit class ResultsFilterChampions(results: Results) {

    def filterChampions(champs: Set[Int]): Results =
      results.copy(
        scalars = results.scalars.map(_.filterChampions(champs)),
        deltas = results.deltas.map(_.filterChampions(champs)),
        derivatives = results.derivatives.map(_.filterChampions(champs))
      )

  }

}
