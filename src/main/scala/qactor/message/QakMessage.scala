package qactor.message

import qactor.AbstractQActor
import qactor.message.InteractionType.InteractionType

object QakMessage {
  private var messageId: Long = 0

  def getNextId: Long = messageId.synchronized {
    val tmp = messageId
    messageId = messageId + 1
    tmp
  }
}

case class QakMessage(interaction: InteractionType, message: Message, from: AbstractQActor, to: AbstractQActor) {
  val id: Long = QakMessage.getNextId

  def serialized: String = s"msg(${message.id}, ${interaction.toString.toLowerCase}, ${from.name}, ${to.name}, ${message.payload}, $id)"

  override def toString: String = serialized
}
