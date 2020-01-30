package prove

import scala.concurrent.duration._
import scala.language.postfixOps
import org.joda.time.DateTime
import qactor.State.{onEnter, onEvent, onMsg, onRequest}
import qactor.context.TcpContext
import qactor.message.{Deserializer, Message}
import qactor.{QActor, State}

object Test extends App {

  Deserializer.registerAll("prove")

  val ctx1 = TcpContext("ctx1", 8021)
  /*.connectWith("localhost", 8018)*/
  val ctx2 = TcpContext("ctx2", 8023).connectWith("localhost", 8021)

  case class Obstacle(distance: Double) extends Message

  case class Hello(name: String) extends Message

  case class GetPosition() extends Message

  case class Position(x: Double, y: Double) extends Message

  case class Cmd(x: String) extends Message

  case class Sonar(x: String) extends Message

  case class Ping() extends Message

  case class Pong() extends Message

  //TODO: remove timeout?
  //TODO: Message can be Any?
  case object DummyActor extends QActor(ctx1) {

    //val pongDispatcher = every(100 millis) dispatch Pong() to mySelf

    override def initialState: State = onEnter {
      println(actualState)
      dispatch(Pong()) to mySelf
      dispatch(Ping()) to mySelf
    } onMsg {
      case Ping() =>
        println("received ping")
        transit to state2
        unstash()
      case _ => stash()
    }

    def state2: State = onEnter {
      println(actualState)
    } onMsg {
      case Pong() => println("finished")
    }

    override def stateChanging(oldState: State, newState: State): Unit = {
      println(s"$oldState -> $newState")
    }


  }

  DummyActor.start()

  case object DummyActor2 extends QActor(ctx1) {

    //val pongDispatcher = every(100 millis) dispatch Pong() to mySelf

    override def initialState: State = onEnter {
      every(3 seconds) request Ping() to PingActor
    } onReply {
      case Ping() =>
        println("received the requested ping")
    }
  }

  DummyActor2.start()

  case object PingActor extends QActor(ctx1) {

    override def initialState: State = onRequest {
      case Ping() => reply(Ping()) to sender
    }
  }

  PingActor.start()

  case object PongActor extends QActor(ctx2) {
    override def initialState: State = dummyStash

    def dummyStash: State = onMsg {
      case _ => stash()
        transit to waiting
    } onExit {
      unstash()
    }

    def waiting: State = onEvent {
      case Ping() =>
        println(DateTime.now.toString("HH:mm:ss") + " Received a event ping")
        transit to pinging
    } onDispatch {
      case Ping() =>
        println(DateTime.now.toString("HH:mm:ss") + " Received a dispatch ping from " + sender)
        transit to pinging
    }

    def pinging: State = onEnter {
      wait(500 millis)
      emit(Pong())
      transit to waiting
    }
  }

  //PongActor.start()


}
