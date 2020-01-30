package qactor.message

import qactor.message.InteractionType.InteractionType


trait Message {
  def id: String = this.getClass.getSimpleName.toLowerCase

  def params: Seq[Any] = this.getClass.getDeclaredFields.filter(_.getName != "$outer").map(f => {
    f.setAccessible(true);
    f.get(this)
  }) //TODO: investigate for $outer

  def payload: String = s"$id(${params.mkString(", ")})"

  def toMessage(interaction: InteractionType, from: String, to: String): QakMessage = QakMessage(interaction, this, from, to)

  override def toString: String = payload
}
