package prove

import java.util.concurrent.{ExecutorService, Executors}

import org.eclipse.californium.core.coap.MediaTypeRegistry
import org.eclipse.californium.core.{CoapClient, CoapHandler, CoapObserveRelation, CoapResponse}

import scala.util.Try

abstract class ResourceSupport(val resourceName: String) {
  def createProperty(name: String, init: String): Boolean

  def setProperty(name: String, value: String): Boolean

  def getProperty(name: String): String
}

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

object PropertyImplicits {
  implicit def propertyValue[T](property: Property[T]): T = property.get
}

trait Property[T] {
  def get: T

  def set(value: T): Unit

  def onChange(body: Property[T] => Unit): Property[T]

  override def toString: String = get.toString
}

trait ObservableProperties[B] {
  def observableProperty[T](init: T, serialize: T => B, deserialize: B => T)(implicit name: sourcecode.Name): Property[T] =
    observableProperty(name.value, init)(serialize, deserialize)

  def observableProperty[T](init: T)(implicit name: sourcecode.Name, serialize: T => B, deserialize: B => T): Property[T] =
    observableProperty(name.value, init)(serialize, deserialize)

  def observableProperty[T](name: String, init: T)(implicit serialize: T => B, deserialize: B => T): Property[T]
}

trait CoapObservableProperties extends ObservableProperties[String] {
  def coapRoot: String

  def coapServer: String

  private val coapSupport = CoapResourceSupport(coapRoot, coapServer)
  private val executor = Executors.newCachedThreadPool()

  case class CoapProperty[T](name: String, init: T, serialize: T => String, deserialize: String => T) extends Property[T] {
    private var value: T = init

    private var observation: Option[CoapObserveRelation] = None
    private var synchronized = false

    initialize()

    private def initialize(): Unit = {
      synchronized = false
      executor.execute(() => {
        observation match {
          case Some(value) => value.reactiveCancel()
          case None =>
        }
        observation = None
        if (coapSupport.createProperty(name, serialize(value))) {
          synchronized = true
          observation = Some(coapSupport.observeProperty(name, v => {
            value = deserialize(v)
            onChangeBody(this)
          }, () => {
            initialize()
          }))
        } else {
          if (!coapSupport.setProperty(name, serialize(value))) {
            coapSupport.initialize() //maybe the server was down
            initialize()
          }
        }
      })
    }

    private var onChangeBody: Property[T] => Unit = _ => {}

    override def onChange(body: Property[T] => Unit): Property[T] = {
      val oldBody = onChangeBody
      onChangeBody =
        p => {
          oldBody(p)
          body(p)
        }
      this
    }

    override def get: T = value

    override def set(value: T): Unit = {
      this.value = value
      onChangeBody(this)
      if (synchronized) {
        executor.execute(() => {
          if (!coapSupport.setProperty(name, serialize(value))) {
            initialize()
          }
        })
      }
    }
  }

  override def observableProperty[T](name: String, init: T)(implicit serialize: T => String, deserialize: String => T): Property[T] =
    CoapProperty(name, init, serialize, deserialize)

  def close(): Unit = executor.shutdown()
}

object TestCoap extends App {

  import PropertyImplicits._

  implicit val intToString = (v: Int) => v.toString
  implicit val stringToInt = (v: String) => v.toInt

  object Actor extends CoapObservableProperties {
    override def coapRoot: String = "actor"

    override def coapServer: String = "coap://localhost:5683"

    val space: Property[Int] = observableProperty(0)

    def run(): Unit = {
      println(space.get)
      while (true) {
        Thread.sleep(1000)
        space.set(10)
        //println(space.get)
        Thread.sleep(1000)
        space.set(space - 3)
        //println(space.get)
      }
    }
  }

  object Actor2 extends CoapObservableProperties {
    override def coapRoot: String = "actor"

    override def coapServer: String = "coap://localhost:5683"

    val space: Property[Int] = observableProperty(0).onChange(p => {
      println("Changed1: " + p.get)
    }).onChange(p => {
      println("Changed2: " + p.get)
    })

    def run(): Unit = {
      /*while (true) {
        println(space.get)
        Thread.sleep(100)
      }*/
    }
  }

  val executor: ExecutorService = Executors.newCachedThreadPool()
  executor.execute(() => Actor.run())

  Actor2.run()
}