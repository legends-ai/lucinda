package asuna.lucinda.dao

import monix.eval.Task
import monix.reactive.OverflowStrategy.DropNew
import monix.reactive.subjects.PublishToOneSubject
import scala.concurrent.duration.Duration

/**
  * @tparam I the key.
  * @tparam O the output.
  */
trait EphemeralDAO[I, O] {
  /**
    * Computes an O.
    */
  def compute(in: I): Task[O]
}

/**
  * Persistent data object. Supports TTLs on data.
  *
  * @tparam I the key.
  * @tparam S the stored value.
  * @tparam O the output.
  */
trait PersistentDAO[I, S, O] extends EphemeralDAO[I, O] {
  /**
    * Fetches this data from the cache.
    */
  def fetch(in: I): Task[Option[S]]

  /**
    * Persists this data to a cache.
    */
  def persist(in: I, data: O): Task[Unit]

  /**
    * Projects the full data to the O.
    */
  def project(full: S): O

  /**
    * Checks if the data is stale. Defaults to never being stale.
    */
  def isStale(stored: S): Boolean = false

  /**
    * Queues up a refresh. Defaults to noop.
    */
  def queueRefresh(in: I): Task[Unit] = Task.unit

  /**
    * Gets data (possibly from cache), persisting it to a cache.
    */
  def get(in: I): Task[O] = {
    fetch(in) flatMap {
      case Some(data) => {
        val update = if (isStale(data)) {
          queueRefresh(in)
        } else {
          Task.unit
        }
        update.map(_ => project(data))
      }
      case None => refresh(in)
    }
  }

  /**
    * Refreshes the element in the cache.
    */
  def refresh(in: I): Task[O] = {
    for {
      data <- compute(in)
      _ <- persist(in, data)
    } yield data
  }

}

/**
  * Allows refreshing of elements in the DAO, in memory.
  */
abstract class RefreshableDAO[I, S, O](
  bufferSize: Int, concurrency: Int, expiryTime: Duration
) extends PersistentDAO[I, S, O] {

  /**
    * The creation time of the object.
    */
  def creation(stored: S): Long

  override def isStale(stored: S): Boolean =
    System.currentTimeMillis - creation(stored) > expiryTime.toMillis

  override def queueRefresh(in: I): Task[Unit] =
    Task.deferFuture(refreshes.onNext(in)).map(_ => ())

  val refreshes = PublishToOneSubject[I]

  /**
    * Initializes the refresher of the DAO.
    */
  def initRefresher: Task[Unit] = {
    refreshes
      // we drop refresh requests that cannot be processed.
      // TODO(igm): add logging for this behavior
      .whileBusyBuffer(DropNew(bufferSize))
      .mapAsync(concurrency)(refresh)
      .foreachL(identity)
  }

}
