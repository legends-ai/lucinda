package asuna.lucinda

import monix.execution.Scheduler

object Main {

  def main(args: Array[String]): Unit = {
    implicit val sched: Scheduler = Scheduler.io()
    new LucindaServer(args).standReady()
  }

}
