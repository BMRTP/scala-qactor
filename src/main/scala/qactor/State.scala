package qactor

import qactor.State.Lazy
import qactor.message.InteractionType._
import qactor.message._

import scala.concurrent.duration.Duration

object State {

  def state(implicit name: sourcecode.Name): String = name.value

  def onEnter(body: => Unit)(implicit name: sourcecode.Name): State = name.value onEnter body

  def onExit(body: => Unit)(implicit name: sourcecode.Name): State = name.value onExit body

  def onQakMsg(body: PartialFunction[QakMessage, Unit])(implicit name: sourcecode.Name): State = name.value onQakMsg body

  def onMsg(body: PartialFunction[Message, Unit])(implicit name: sourcecode.Name): State = name.value onMsg body

  def onInteraction(body: PartialFunction[(InteractionType, Message), Unit])(implicit name: sourcecode.Name): State = name.value onInteraction body

  def onEvent(body: PartialFunction[Message, Unit])(implicit name: sourcecode.Name): State = name.value onEvent body

  def onDispatch(body: PartialFunction[Message, Unit])(implicit name: sourcecode.Name): State = name.value onDispatch body

  def onRequest(body: PartialFunction[Message, Unit])(implicit name: sourcecode.Name): State = name.value onRequest body

  def onReply(body: PartialFunction[Message, Unit])(implicit name: sourcecode.Name): State = name.value onReply body

  def timeout(duration: (Duration, Lazy[Unit]))(implicit name: sourcecode.Name): State = name.value timeout duration

  implicit class stringToEmptyState(v: String) extends State(v, None, None, None, None)

  implicit class Lazy[T](wrp: => T) {
    lazy val value: T = wrp
  }
}



case class State(name: String, enterAction: Option[() => Unit],
                 exitAction: Option[() => Unit],
                 onMessageAction: Option[PartialFunction[QakMessage, Unit]],
                 timeoutV: Option[(Duration, Lazy[Unit])]) {

  override def toString: String = name

  def timeout(duration: (Duration, Lazy[Unit])): State = this.copy(timeoutV = Some(duration)) //TODO keep smallest

  def onEnter(body: => Unit): State = this.copy(enterAction = Some(() => {
    this.enterAction.foreach(body => body())
    body
  }))

  def onExit(body: => Unit): State = this.copy(exitAction = Some(() => {
    this.exitAction.foreach(body => body())
    body
  }))

  def onQakMsg(body: PartialFunction[QakMessage, Unit]): State = this.copy(onMessageAction = this.onMessageAction match {
    case Some(value) => Some(value.orElse(body))
    case None => Some(body)
  })

  def onMsg(body: PartialFunction[Message, Unit]): State = this onQakMsg {
    case QakMessage(_, payload, _, _) if body.isDefinedAt(payload) => body(payload)
  }

  def onInteraction(body: PartialFunction[(InteractionType, Message), Unit]): State = this onQakMsg {
    case QakMessage(interaction, payload, _, _) if body.isDefinedAt(interaction, payload) => body(interaction, payload)
  }

  def onEvent(body: PartialFunction[Message, Unit]): State = this onInteraction {
    case (Event, payload) if body.isDefinedAt(payload) => body(payload)
  }

  def onDispatch(body: PartialFunction[Message, Unit]): State = this onInteraction {
    case (Dispatch, payload) if body.isDefinedAt(payload) => body(payload)
  }

  def onRequest(body: PartialFunction[Message, Unit]): State = this onInteraction {
    case (Request, payload) if body.isDefinedAt(payload) => body(payload)
  }

  def onReply(body: PartialFunction[Message, Unit]): State = this onInteraction {
    case (Reply, payload) if body.isDefinedAt(payload) => body(payload)
  }


  def and(toMerge: State): State = {

    val s1 = toMerge.onMessageAction match {
      case Some(value) => this.onQakMsg(value)
      case None => this
    }

    val s2 = toMerge.enterAction match {
      case Some(value) => s1.onEnter(value())
      case None => s1
    }

    val s3 = toMerge.exitAction match {
      case Some(value) => s2.onExit(value())
      case None => s2
    }

    s3.copy(name = this.name + " and " + toMerge.name)
  }
}