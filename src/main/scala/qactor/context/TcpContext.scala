package qactor.context

import java.io.{BufferedReader, DataOutputStream, InputStreamReader}
import java.net.{ServerSocket, Socket}

import qactor.message._

import scala.util.Try

case class TcpContext(override val contextName: String, port: Int) extends Context {

  private val welcomeSocket = new ServerSocket(port)
  private var connections = Map[String, (Socket, DataOutputStream)]()
  private val connectionsLock = new Object()

  executor.execute { () =>
    while (true) {
      handleSocket(welcomeSocket.accept())
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
        Deserializer(msg) match {
          case Some(value) => digest(value)
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
