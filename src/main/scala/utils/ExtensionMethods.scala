package utils

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, ScheduledFuture, TimeUnit}

import scala.concurrent.duration.Duration

object ExtensionMethods {

  implicit class RichExecutorService(executor: ExecutorService) {
    private def execute(body: () => Unit): Unit = executor.submit(new Runnable {
      override def run(): Unit = body()
    })
  }

  implicit class RichScheduledExecutorService(executor: ScheduledExecutorService) {
    def schedule(body: () => Unit, time: Duration): ScheduledFuture[_] = executor.schedule(new Runnable {
      override def run(): Unit = body()
    }, time.toMillis, TimeUnit.MILLISECONDS)

    def scheduleAtFixedRate(body: () => Unit, initialDelay: Duration, period: Duration): ScheduledFuture[_] = executor.scheduleAtFixedRate(
      () => body(), initialDelay.toMillis, period.toMillis, TimeUnit.MILLISECONDS)
  }

  implicit class RickIntTuple(c:(Int,Int)) {
    def neighbors: Seq[(Int, Int)] = c match {
      case (x, y) => Seq((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1))
    }
  }

}