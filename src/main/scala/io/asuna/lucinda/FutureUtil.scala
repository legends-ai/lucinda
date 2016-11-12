package io.asuna.lucinda

import scala.concurrent.{ ExecutionContext, Future }

object FutureUtil {

  def sequenceMap[S, T](map: Map[S, Future[T]])(implicit ec: ExecutionContext): Future[Map[S, T]] = {
    Future.sequence(map.map { case (k, fut) =>
      fut.map(v => (k, v))
    }).map(_.toMap)
  }

}
