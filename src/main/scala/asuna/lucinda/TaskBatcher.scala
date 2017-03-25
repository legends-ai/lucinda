package asuna.lucinda

import java.util.concurrent.ConcurrentHashMap
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.subjects.PublishToOneSubject
import scala.concurrent.Promise
import scala.util.{ Failure, Success }

/**
  * Bidirectional task batcher.
  * TODO(igm): allow specifying throughput
  */
abstract class TaskBatcher[I, O](concurrency: Int) {
  import TaskBatcher.Element

  /**
    * Processes a single element.
    */
  def process(in: I): Task[O]

  final val queue = PublishToOneSubject[Element[I, O]]

  /**
    * Elements currently being processed.
    */
  final val processing = new ConcurrentHashMap[I, Element[I, O]]()

  /**
    * Enqueues a fetch in the task batcher.
    */
  final def enqueue(in: I): Task[O] = Task.deferFutureAction { implicit sched: Scheduler =>
    Option(processing.get(in)) match {
      case Some(el) => el.promise.future
      case None => {
        val promise = Promise[O]()
        queue.onNext(Element(in, promise)).flatMap { _ =>
          promise.future
        }
      }
    }
  }

  /**
    * Starts processing elements.
    */
  final def start: Task[Unit] = {
    queue
      .mapAsync(concurrency) { el =>
        val task = for {
          result <- process(el.in).materialize
          _ = processing.remove(el)
        } yield result
        task.map {
          case Success(x) => el.promise.success(x)
          case Failure(err) => el.promise.failure(err)
        }
      }
      .foreachL(identity)
  }

}

object TaskBatcher {

  case class Element[I, O](in: I, promise: Promise[O])

}
