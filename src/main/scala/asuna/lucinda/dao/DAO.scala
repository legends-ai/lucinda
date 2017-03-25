package asuna.lucinda.dao

import asuna.lucinda.TaskBatcher
import com.timgroup.statsd.StatsDClient
import java.util.concurrent.ConcurrentHashMap
import com.google.protobuf.timestamp.Timestamp
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import monix.reactive.OverflowStrategy.DropNew
import monix.reactive.subjects.PublishToOneSubject
import asuna.lucinda.DAOSettings
import scala.concurrent.duration.Duration
import cats.implicits._

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
  * TODO(igm): rethink this class hierarchy
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
    * @param forceRefresh Forces refreshing of data.
    */
  def get(in: I, forceRefresh: Boolean = false): Task[O] = {
    fetch(in) flatMap {
      case Some(data) if !forceRefresh => {
        val update = if (isStale(data)) {
          queueRefresh(in)
        } else {
          Task.unit
        }
        update.map(_ => project(data))
      }
      case _ => refresh(in)
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
  settings: DAOSettings
)(implicit statsd: StatsDClient) extends PersistentDAO[I, S, O] {

  val refreshes = PublishToOneSubject[I]

  val refreshing = new ConcurrentHashMap[I, Unit]

  // use io scheduler to refresh since this makes lots of DB calls.
  val scheduler = Scheduler.io(name = settings.name)

  /**
    * The creation time of the object.
    */
  def creation(stored: S): Long

  override def isStale(stored: S): Boolean =
    System.currentTimeMillis - creation(stored) > settings.expiryTime.toMillis

  override def queueRefresh(in: I): Task[Unit] = {
    if (refreshing.containsKey(in)) {
      Task.unit
    } else {
      refreshing.put(in, ())
      Task.deferFuture(refreshes.onNext(in)).map(_ => ())
    }
  }

  /**
    * Initializes the refresher of the DAO.
    */
  def initRefresher: Task[Unit] = {
    refreshes
      // we drop refresh requests that cannot be processed.
      // TODO(igm): add logging for this behavior
      .whileBusyBuffer(DropNew(settings.bufferSize))
      .mapAsync(settings.refreshConcurrency) { el =>
        refresh(el) map { out =>
          refreshing.remove(el)
          out
        }
      }
      .foreachL(identity)
 }

  def startRefreshing: Future[Unit] = {
    initRefresher.runAsync(scheduler)
  }

  override def refresh(in: I): Task[O] = {
    val time = System.currentTimeMillis()
    for {
      result <- super.refresh(in)
      _ = statsd.recordExecutionTime(
        s"dao.refresh.${settings.name}",
        System.currentTimeMillis() - time
      )
    } yield result
  }

  /**
    * The batcher batches processing of equal inputs.
    */
  val batcher = new TaskBatcher[I, O](settings.batchConcurrency) {
    override def process(in: I): Task[O] = get(in)
  }

  /**
    * Gets data (possibly from cache), persisting it to a cache.
    * @param forceRefresh Forces refreshing of data.
    */
  override def get(in: I, forceRefresh: Boolean = false): Task[O] = {
    fetch(in) flatMap {
      case Some(data) if !forceRefresh => {
        val update = if (isStale(data)) {
          batcher.enqueue(in)
        } else {
          Task.unit
        }
        update.map(_ => project(data))
      }
      case _ => refresh(in)
    }
  }

}

abstract class RefreshableProtoDAO[I, S, O](
  settings: DAOSettings
)(implicit statsd: StatsDClient) extends RefreshableDAO[I, S, O](settings) {

  def creationTs(stored: S): Option[Timestamp]

  override def creation(stored: S): Long = {
    creationTs(stored).map { ts =>
      (ts.seconds * 1000 + (ts.nanos / 1E6)).toLong
    }.orEmpty
  }

}
