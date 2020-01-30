package qactor.message

import qactor.message.InteractionType.InteractionType

object QakMessage {
  private var messageId: Long = 0

  def getNextId: Long = messageId.synchronized {
    val tmp = messageId
    messageId = messageId + 1
    tmp
  }
}

case class QakMessage(interaction: InteractionType, message: Message, from: String, to: String) {
  val id: Long = QakMessage.getNextId

  def serialized: String = s"msg(${message.id}, ${interaction.toString.toLowerCase}, $from, $to, ${message.payload}, $id)"

  override def toString: String = serialized
}
