package supports.properties

import java.util.concurrent.Executors

import org.eclipse.californium.core.CoapObserveRelation
import supports.resource.CoapResourceSupport

trait CoapObservableProperties extends ObservableProperties[String] {
  def coapRoot: String

  def coapServer: String

  private val coapSupport = CoapResourceSupport(coapRoot, coapServer)
  private val executor = Executors.newCachedThreadPool()
  private val singleExecutor = Executors.newSingleThreadExecutor()

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
        singleExecutor.execute(() => {
          if (!coapSupport.setProperty(name, serialize(value))) {
            initialize()
          }
        })
      }
    }
  }

  override def observableProperty[T](name: String, init: T)(implicit serialize: T => String, deserialize: String => T): Property[T] =
    CoapProperty(name, init, serialize, deserialize)

  def close(): Unit = {
    executor.shutdown()
    singleExecutor.shutdown()
  }
}
