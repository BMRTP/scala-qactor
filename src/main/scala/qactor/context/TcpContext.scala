package qactor.context

import java.io.{BufferedReader, DataOutputStream, InputStreamReader}
import java.net.{ServerSocket, Socket}

import qactor.QActor
import qactor.message._

import scala.util.Try

object TcpContext {
  def apply(contextName: String): TcpContext = TcpContext(contextName, 0)

  def apply()(implicit contextName: sourcecode.Name): TcpContext = TcpContext(contextName.value, 0)

  def apply(port: Int)(implicit contextName: sourcecode.Name): TcpContext = TcpContext(contextName.value, port)
}
case class TcpContext(override val contextName: String, port: Int) extends Context {

  private var connections = Map[String, (Socket, DataOutputStream)]()
  private val connectionsLock = new Object()
  private var extensions: Seq[TcpContext] = Seq()

  if (port != 0) {
    val welcomeSocket = new ServerSocket(port)
    executor.execute { () =>
      while (true) {
        handleSocket(welcomeSocket.accept())
      }
    }
  }

  def connectWith(hostname: String, port: Int): TcpContext = { //TODO: add to a connection list, retry if can't connect
    executor.execute(() => {
      Try {
        val otherContext = new Socket(hostname, port)
        handleSocket(otherContext)
      } recover {
        case _ => println(s"$contextName: can't connect to $hostname:$port")
      }
    })
    this
  }

  def extendTo(hostname: String, port: Int): TcpContext = {
    val ctx = TcpContext(contextName + " extension").connectWith(hostname, port)
    extensions = extensions :+ ctx
    ctx
  }

  override def register(qactor: QActor): Unit = {
    extensions.foreach(x => x.register(qactor))
    super.register(qactor)
  }

  override def unregister(qactor: QActor): Unit = {
    extensions.foreach(x => x.unregister(qactor))
    super.unregister(qactor)
  }

  private def handleSocket(socket: Socket): Unit = {
    val connectionId = socket.getRemoteSocketAddress.toString
    println(s"$contextName: new connection established with $connectionId")
    Try {

      val outToContext = new DataOutputStream(socket.getOutputStream)
      connectionsLock.synchronized {
        connections = connections + (connectionId -> (socket, outToContext))
      }
      val inFromContext = new BufferedReader(new InputStreamReader(socket.getInputStream))

      var continue = true
      while (continue) {
        val msg = inFromContext.readLine()
        Deserializer(msg, this) match {
          case Some(value) => if (!digest(value)) {
            println(s"$contextName: message $value dropped")
          }
          case None =>
            println(s"$contextName: a message from $connectionId was malformed ($msg)")
            dropConnection(connectionId)
            continue = false
        }
      }
    } recover {
      case _ => dropConnection(connectionId)
    }
  }

  private def emitMessage(message: QakMessage): Unit = connections.foreach {
    connection =>
      executor.execute { () =>
        Try {
          val msg = message.toString + "\n"
          connection._2._1 synchronized {
            connection._2._2.writeBytes(msg)
            connection._2._2.flush()
          }
        } recover {
          case _ => dropConnection(connection._1)
        }
      }
  }

  private def dropConnection(connection: String): Unit = if (connections contains connection) {
    println(s"$contextName: the connection with $connection was closed")
    connections = connections - connection
  }

  override def handle(message: QakMessage): Unit = {
    if (!digest(message)) {
      emitMessage(message)
    }
  }

  override def close(): Unit = {
    connections.foreach {
      connection =>
        executor.execute { () =>
          Try {
            connection._2._1.close()
          } recover {
            case _ =>
          }
        }
    }
    super.close()
  }
}
