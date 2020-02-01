package qactor.context

import java.util.concurrent.{ExecutorService, Executors}

import qactor.QActor
import qactor.message.InteractionType._
import qactor.message._

trait Context {
  private var localQActor: Map[String, QActor] = Map()
  private val localQActorLock: Object = new Object()
  protected val executor: ExecutorService = Executors.newCachedThreadPool()

  def contextName: String

  def register(qactor: QActor): Unit = localQActorLock.synchronized {
    localQActor = localQActor + (qactor.name -> qactor)
  }

  def unregister(qactor: QActor): Unit = localQActorLock.synchronized {
    localQActor = localQActor - qactor.name
  }

  def handle(message: QakMessage)

  protected def digest(message: QakMessage): Boolean =
    message match {
      case QakMessage(Event, _, _, _) =>
        localQActor.filter(_._1 != message.from.name).foreach(_._2.accept(message))
        false
      case _ =>
        val dest = localQActor.filter(_._1 == message.to.name).take(1)
        if (dest.isEmpty) {
          //println(contextName + ": " + message + " has no destination in this context")
          false
        } else {
          dest.foreach(_._2.accept(message))
          true
        }
    }


  def close(): Unit = executor.shutdown()
}
