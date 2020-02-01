import Messages._
import org.scalatest._
import qactor.State._
import qactor.context._
import qactor.message._
import qactor.{QActor, State}

import scala.concurrent.duration._
import scala.language.postfixOps

object Messages {

  case class Ping() extends Message

  case class Pong() extends Message

  case class TestDone() extends Message

  case class PoisonPill() extends Message

}

class QActorTests extends FunSuite {

  Deserializer.registerAll("Messages")

  val localCtx: Context = LocalContext()
  val ctx1: TcpContext = TcpContext("ctx1", 8021)
  val ctx2: TcpContext = TcpContext("ctx2", 8023).connectWith("localhost", 8021)

  trait Received {
    def received: Int
  }

  def pongActor(ctx: Context): QActor with Received = new QActor("pongActor", ctx) with Received {

    def received: Int = _received

    var _received = 0

    override def initialState: State = onEnter {
      println(s"$name started")
    } onExit {
      emit {
        Pong()
      }
    } timeout ((500 millis) -> {
      transit to waiting
    })


    def waiting: State = onMsg {
      case Ping() =>
        println(s"$name received a ping... ")
        _received = _received + 1
        transit to pinging
    }

    def pinging: State = onEnter {
      if (received < 10) {
        wait(5 millis)
        println("ponging")
        emit(Pong())
        transit to waiting
      } else {
        transit to onEnter {
          println("done")
          emit {
            TestDone()
          }
          transit to watcher
        }
      }
    }

    def watcher: State = onMsg {
      case PoisonPill() => stop()
    }
  }

  def pingActor(ctx: Context): QActor with Received = new QActor("pingActor", ctx) with Received {

    def received: Int = _received

    var _received = 0

    override def initialState: State = onEnter {
      println(s"$name started")
      transit to (pinging and watcher)
    }

    def pinging: State = onMsg {
      case Pong() =>
        println(s"$name received a pong... ")
        _received = _received + 1
        wait(5 millis)
        println("pinging")
        dispatch(Ping()) to sender
        transit to (pinging and watcher)
    }

    def watcher: State = onMsg {
      case PoisonPill() => stop()
    }

  }

  def runPingPongTest(ctx1: Context, ctx2: Context): Unit = {
    val pong = pongActor(ctx1)
    val ping = pingActor(ctx2)
    pong.start()
    ping.start()

    case object WaiterActor extends QActor(ctx1) {
      override def initialState: State = onMsg {
        case TestDone() =>
          emit {
            PoisonPill()
          }
          assert(ping.received == pong.received && ping.received == 10)
          WaiterActor.stop()
          this.synchronized {
            this.notify()
          }
      }
    }

    WaiterActor.start()
    WaiterActor.synchronized {
      WaiterActor.wait()
    }

  }

  test("QActor behaviour") {
    0 until 4 foreach (_ => {
      runPingPongTest(localCtx, localCtx)
      runPingPongTest(ctx1, ctx2)
      runPingPongTest(ctx1, ctx1)
    })
  }


  test("QActor behaviour (timeouts)") {
    case object DummyActor extends QActor(localCtx) {

      override def initialState: State = onEnter {
        println("test1")
        in(1 second) dispatch Ping() to mySelf
      } onMsg {
        case Ping() =>
          println("received ping")
          wait(2 second)
          transit to ok
      } timeout ((2 second) -> {
        transit to notOk
      })

      def ok: State = onEnter {
        println("ok")
        assert(true)
        transit to test2
      }

      def test2: State = onEnter {
        println("test2")
        in(2 second) dispatch Ping() to mySelf
      } onMsg {
        case Ping() =>
          assert(false)
          transit to close
      } timeout ((1 second) -> {
        transit to ok2
      })

      def ok2: State = onEnter {
        println("ok")
        assert(true)
      } onMsg {
        case Ping() =>
          assert(true)
          transit to close
      }

      def notOk: State = onEnter {
        println("not ok")
        assert(false)
        transit to close
      }

      def close: State = onEnter {
        this.synchronized {
          this.notify()
        }
        stop()
      }
    }

    DummyActor.start()
    DummyActor.synchronized {
      DummyActor.wait()
    }
  }

}