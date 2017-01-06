package io.asuna.lucinda

import scala.concurrent.ExecutionContext

object Main {

  def main(args: Array[String]): Unit = {
    new LucindaServer(args).standReady()
  }

}
