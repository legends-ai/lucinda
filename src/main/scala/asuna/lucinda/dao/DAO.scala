package asuna.lucinda.dao

import asuna.lucinda._
import cats.implicits._
import com.timgroup.statsd.StatsDClient
import com.google.protobuf.timestamp.Timestamp
import java.util.concurrent.ConcurrentHashMap
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.OverflowStrategy.DropNew
import monix.reactive.subjects.PublishToOneSubject
import scala.concurrent.Future

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
  * Allows refreshing of elements in the DAO, in memory.
  */
abstract class RefreshableDAO[I, S, O](
  settings: DAOSettings
)(implicit statsd: StatsDClient) extends EphemeralDAO[I, O] {

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

  val refreshes = PublishToOneSubject[I]

  val refreshing = new ConcurrentHashMap[I, Unit]

  // use io scheduler to refresh since this makes lots of DB calls.
  val scheduler = Scheduler.io(name = settings.name)

  /**
    * The creation time of the object.
    */
  def creation(stored: S): Long

  /**
    * Checks if the data is stale.
    */
  def isStale(stored: S): Boolean =
    System.currentTimeMillis - creation(stored) > settings.expiryTime.toMillis

  /**
    * Queues up a refresh. Defaults to noop.
    */
  def queueRefresh(in: I): Task[Unit] = {
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
    Task
      .gatherUnordered(List(initRefresher, batcher.start))
      .map(_ => ())
      .runAsync(scheduler)
  }

  /**
    * Refreshes the element in the cache.
    */
  def refresh(in: I): Task[O] = {
    val time = System.currentTimeMillis()
    for {
      result <- compute(in)
      _ <- persist(in, result)

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
    override def process(in: I): Task[O] = refresh(in)
  }

  /**
    * Gets data (possibly from cache), persisting it to a cache.
    * @param forceRefresh Forces refreshing of data.
    */
  def get(in: I, forceRefresh: Boolean = false): Task[O] = {
    fetch(in) flatMap {
      case Some(data) if !(forceRefresh || settings.cacheBypass) => {
        val update = if (isStale(data)) {
          queueRefresh(in)
        } else {
          Task.unit
        }
        update.map(_ => project(data))
      }
      case _ => batcher.enqueue(in)
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
