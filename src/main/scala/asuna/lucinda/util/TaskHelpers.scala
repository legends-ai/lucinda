package asuna.lucinda.util

import cats.{ Cartesian, Traverse }
import monix.eval.Task
import scala.language.higherKinds
import cats.implicits._


object TaskHelpers {

  implicit class MapTask[K, V](t: Map[K, V]) {
    def traverseG[U](fn: V => Task[U]): Task[Map[K, U]] = {
      val list = t.mapValues(fn).toList
      Task.gather(list.map(_._2)).map { results =>
        list.map(_._1).zip(results).toMap
      }
    }
  }

  implicit class ListTraverse[V](t: List[V]) {
    def traverseG[U](fn: V => Task[U]): Task[List[U]] = {
      Task.gather(t.map(fn))
    }
  }

  implicit class ListSequence[V](t: List[Task[V]]) {
    def sequenceG: Task[List[V]] = t.traverseG(identity)
  }

}
