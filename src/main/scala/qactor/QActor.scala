package qactor

import java.util.concurrent._

import qactor.State._
import qactor.context._
import qactor.message.InteractionType._
import qactor.message._
import utils.ExtensionMethods._

import scala.concurrent.duration.Duration
import scala.util.control.Breaks._

object QActor {
  val MAX_QUEUE_LENGTH: Int = 10000

  implicit def qactorToString(qactor: QActor) = qactor.name
}

abstract class QActor(final val name: String, private val context: Context) extends AbstractQActor {

  def this(context: Context)(implicit name: sourcecode.Name) {
    this(name.value.toLowerCase, context)
  }

  private trait AcceptedEvent

  private case class InternalTimeout(body: Lazy[Unit], executionStage: Long) extends AcceptedEvent

  private case class IngoingMessage(message: QakMessage) extends AcceptedEvent

  private var currentState: State = "dead"
  private val executor: ExecutorService = Executors.newSingleThreadExecutor()
  private val timerExecutor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  private var timeoutFuture: Option[ScheduledFuture[_]] = None
  private var nextState: Option[(State, Boolean)] = None //(State, isBackwardTransition)
  private var transitionEnabled = true
  private val queue: scala.collection.mutable.Queue[AcceptedEvent] = scala.collection.mutable.Queue[AcceptedEvent]()
  private val stateStack: scala.collection.mutable.Stack[State] = scala.collection.mutable.Stack[State]()

  private val queueSemaphore = new Semaphore(0)
  private var living: Boolean = false
  private var currentMessage: Option[QakMessage] = None
  private var executionStage: Long = 0

  /** * To override ***/
  protected def initialState: State

  /** * Life controls ***/
  final def start(): Unit = {
    def startQactor(): Unit = {
      living = true
      context.register(this)
      transit to initialState
      gotoNextStateIfPresent()
    }

    def consumeMessage(): Unit = {
      queueSemaphore.acquire(1)
      if (living) {
        val message = queue.synchronized {
          queue.dequeue
        }
        message match {
          case InternalTimeout(body, es) => if (es == executionStage) {
            breakable {
              body.value
            }
            gotoNextStateIfPresent()
          }
          case IngoingMessage(m) =>
            currentMessage = Some(m)
            currentState.onMessageAction match {
              case Some(body) if body isDefinedAt m =>
                breakable {
                  body(m)
                }
                currentMessage = None
                gotoNextStateIfPresent()
              case _ =>
                currentMessage = None
                println(name + ": " + m + " was dropped on state " + currentState)
            }
        }
      }
    }

    executor.execute { () =>
      startQactor()
      while (living) {
        consumeMessage()
      }
    }
  }

  protected final def stop(): Unit = {
    context.unregister(this)
    timeoutFuture.foreach(_.cancel(false))
    executor.shutdown()
    timerExecutor.shutdown()
    living = false
    queueSemaphore.release(1)
    println(s"$name stopped")
  }

  /** * State transitions ***/

  case class Back() extends Object

  case class Now() extends Object

  trait Transitor {
    def to(state: State): Unit

    def to(back: Back): Unit
  }

  protected final def transit: Transitor = new Transitor {
    private def transitIfPossible(state: State, isBackwardTransition: Boolean): Unit = if (transitionEnabled) {
      nextState = Some(state, isBackwardTransition)
    } else {
      throw new Exception("Transitions are disabled while exiting")
    }

    override def to(state: State): Unit = transitIfPossible(state, isBackwardTransition = false)

    override def to(back: Back): Unit = transitIfPossible(stateStack.pop(), isBackwardTransition = true)
  }

  protected final def previous: Back = Back()

  protected final def interruptAndTransit(): Unit = if (nextState.isDefined) {
    break()
  } else {
    throw new Exception("You must first specify a transition")
  }

  protected final def canTransitBack: Boolean = stateStack.nonEmpty

  @scala.annotation.tailrec
  private def gotoNextStateIfPresent(): Unit = {
    def applyTransitionTo(state: State, isBackwardTransition: Boolean): Unit = {
      if (isBackwardTransition) {
        //stateStack.pop()
      }
      cancelTimeout()
      transitionEnabled = false
      currentState.exitAction.foreach(body => body())
      if (!isBackwardTransition) {
        stateStack.push(currentState)
      }
      stateChanging(currentState, state)
      transitionEnabled = true
      currentState = state
      executionStage = executionStage + 1
      breakable {
        state.enterAction.foreach(body => body())
      }
      timeoutFuture = setTimeout(state)
    }

    val tmp = nextState
    nextState = None
    tmp match {
      case Some((state, isBackwardTransition)) =>
        applyTransitionTo(state, isBackwardTransition)
        gotoNextStateIfPresent()
      case None =>
    }
  }

  protected def stateChanging(oldState: State, newState: State): Unit = ()

  /** * State timeout ***/
  protected final def cancelTimeout(): Unit = timeoutFuture.foreach(_.cancel(false))

  protected final def resetTimeout(): Unit = {
    cancelTimeout()
    timeoutFuture = setTimeout(state)
  }

  private def setTimeout(state: State): Option[ScheduledFuture[_]] = state.timeoutV.map {
    case (duration, body) =>
      timerExecutor.schedule(() => {
        timeoutFuture = None
        acceptTimeout(body)
      }, duration)
  }

  /** * Message accepting ***/

  final def accept(message: QakMessage): Unit = accept(IngoingMessage(message))

  private def acceptTimeout(body: Lazy[Unit]): Unit = accept(InternalTimeout(body, executionStage))

  private def accept(event: AcceptedEvent): Unit =
    queue.synchronized {
      val dropEvent = event match {
        case _: IngoingMessage => queue.size >= QActor.MAX_QUEUE_LENGTH
        case _: InternalTimeout => false
      }
      if (dropEvent) {
        println(s"$name: event $event was dropped because the queue is full")
      } else {
        queue.enqueue(event)
        queueSemaphore.release(1)
      }
    }

  /** * Message forwarding ***/

  private def forward(messageType: InteractionType, metaMessage: Message, to: AbstractQActor): Unit = to.accept(QakMessage(messageType, metaMessage, this, to))

  case class InstantForwarder(metaMessage: Message, messageType: InteractionType) {
    def to(to: AbstractQActor): Unit = forward(messageType, metaMessage, to)
  }

  protected final def emit(metaMessage: Message): Unit = forward(Event, metaMessage, ExternalQActor("none", context))

  protected final def dispatch(metaMessage: Message): InstantForwarder = InstantForwarder(metaMessage, Dispatch)

  protected final def request(metaMessage: Message): InstantForwarder = InstantForwarder(metaMessage, Request)

  protected final  def reply(metaMessage: Message): InstantForwarder = InstantForwarder(metaMessage, Reply)

  trait Cancellable {
    def stop(): Unit
  }

  protected final def in(duration: Duration) = new {
    case class DelayedForwarder(in: Duration, metaMessage: Message, messageType: InteractionType) {
      def to(to: AbstractQActor): Cancellable = {
        val future = timerExecutor.schedule(() => forward(messageType, metaMessage, to), in)
        () => future.cancel(false)
      }
    }

    def emit(metaMessage: Message): Cancellable = {
      val future = timerExecutor.schedule(() => forward(Event, metaMessage, ExternalQActor("none", context)), duration)
      () => future.cancel(false)
    }

    def dispatch(metaMessage: Message): DelayedForwarder = DelayedForwarder(duration, metaMessage, Dispatch)

    def request(metaMessage: Message): DelayedForwarder = DelayedForwarder(duration, metaMessage, Request)

    def reply(metaMessage: Message): DelayedForwarder = DelayedForwarder(duration, metaMessage, Reply)
  }

  protected final def every(duration: Duration) = new {

    case class FixedTimeForwarder(every: Duration, metaMessage: Message, messageType: InteractionType) {
      def to(to: AbstractQActor): Cancellable = {
        val future = timerExecutor.scheduleAtFixedRate(() => forward(messageType, metaMessage, to), every, every)
        () => future.cancel(false)
      }
    }

    def emit(metaMessage: Message): Cancellable = {
      val future = timerExecutor.scheduleAtFixedRate(() => forward(Event, metaMessage, ExternalQActor("none", context)), duration, duration)
      () => future.cancel(false)
    }

    def dispatch(metaMessage: Message): FixedTimeForwarder = FixedTimeForwarder(duration, metaMessage, Dispatch)

    def request(metaMessage: Message): FixedTimeForwarder = FixedTimeForwarder(duration, metaMessage, Request)

    def reply(metaMessage: Message): FixedTimeForwarder = FixedTimeForwarder(duration, metaMessage, Reply)
  }

  /** * Message stashing ***/
  private val stashed: scala.collection.mutable.Queue[QakMessage] = scala.collection.mutable.Queue[QakMessage]()

  protected final def stash(): Unit = stashed.synchronized {
    currentMessage.foreach(v => stashed.enqueue(v))
  }

  protected final def unstash(): Unit = stashed.synchronized {
    stashed.foreach(v => accept(v));
    stashed.clear()
  }

  /** * Utilities ***/
  protected final def actualState: State = currentState

  protected final def mySelf: QActor = this

  protected final def wait(duration: Duration): Unit = Thread.sleep(duration.toMillis)

  protected final def sender: AbstractQActor = currentMessage match {
    case Some(value) => value.from
    case None => new AbstractQActor {
      override def name: String = "none"

      override def accept(message: QakMessage): Unit = {}
    }
  }

  protected final def stackOfStates: Seq[State] = stateStack.toSeq

  protected final def resetStackOfStates(): Unit = stateStack.clear()
}