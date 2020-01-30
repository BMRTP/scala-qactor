package qactor.context

import qactor.message._

case class LocalContext(override val contextName: String = "localContext") extends Context {

  override def handle(message: QakMessage): Unit = digest(message)
}
