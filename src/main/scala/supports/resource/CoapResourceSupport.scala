package supports.resource

import org.eclipse.californium.core.coap.MediaTypeRegistry
import org.eclipse.californium.core.{CoapClient, CoapHandler, CoapObserveRelation, CoapResponse}

import scala.util.Try


case class CoapResourceSupport(override val resourceName: String, serverAddr: String) extends ResourceSupport(resourceName) {

  initialize()

  def initialize(): Unit = {
    performCoapOperation("resources")(_.post(resourceName, MediaTypeRegistry.TEXT_PLAIN))
  }

  private def getClient(path: String): CoapClient = {
    val url = serverAddr + "/" + path
    val client = new CoapClient(url)
    client.setTimeout(1000L)
    client
  }

  private def performCoapOperation[T](path: String)(operation: CoapClient => T): Option[T] = {
    Try {
      val client = getClient(path)
      val ret = operation(client)
      client.shutdown()
      ret
    }.toOption
  }

  def observeProperty(name: String, onLoadHandler: String => Unit, onErrorHandler: () => Unit): CoapObserveRelation = {
    val client = getClient(s"$resourceName/$name")

    client.observe(new CoapHandler {
      override def onLoad(response: CoapResponse): Unit = onLoadHandler(response.getResponseText)

      override def onError(): Unit = onErrorHandler()
    })
  }

  override def createProperty(name: String, init: String): Boolean = {
    performCoapOperation(resourceName)(_.post(name, MediaTypeRegistry.TEXT_PLAIN).isSuccess).getOrElse(false)
    setProperty(name, init)
  }

  override def setProperty(name: String, value: String): Boolean =
    performCoapOperation(s"$resourceName/$name")(_.put(value, MediaTypeRegistry.TEXT_PLAIN).isSuccess).getOrElse(false)

  override def getProperty(name: String): String =
    performCoapOperation(s"$resourceName/$name")(_.get().getResponseText).getOrElse("")
}
