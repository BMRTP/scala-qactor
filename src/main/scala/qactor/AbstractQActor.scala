package qactor

import qactor.context.Context
import qactor.message.QakMessage

abstract class AbstractQActor {
  def name: String

  def accept(message: QakMessage): Unit
}

object ExternalQActor {
  def apply(context: Context)(implicit name: sourcecode.Name): ExternalQActor = {
    ExternalQActor (name.value.toLowerCase, context)
  }
}

case class ExternalQActor(name: String, context: Context) extends AbstractQActor {
  override def accept(message: QakMessage): Unit = context.handle(message)

  override def toString: String = name
}