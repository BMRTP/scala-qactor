package prove

import prove.Messages._
import qactor.State._
import qactor.context.TcpContext
import qactor.message.Deserializer
import qactor.{ExternalQActor, QActor, State}

import scala.concurrent.duration._
import scala.language.postfixOps

object DetectorApp extends App {

  Deserializer.registerAll("prove.Messages")

  val ctxDetector = TcpContext(8023)
  val ctxRobot = ctxDetector.extendTo("localhost", 8018)
  val ctxPlasticBox = ctxDetector.extendTo("localhost", 8016)

  val smartrobot = ExternalQActor(ctxRobot)
  val grabber = ExternalQActor(ctxRobot)
  val obstacleclassifier = ExternalQActor(ctxRobot)
  val plasticbox = ExternalQActor(ctxPlasticBox)

  val planner: MatrixPlanner = new MatrixPlanner(
    robotInitialPosition = ((0, 0), South),
    robotEmptyPosition = ((0, 0), North)) with DummyMatrixPlanner


  object Detector extends QActor(ctxDetector) {

    val MAX_BOTTLE = 1
    var currentBottlesCount = 0

    var beforeEmptyPos: Option[((Int, Int), Pole)] = None
    var lastObstacle: Option[Obstacle] = None
    var lastStepFailTime: Int = 0
    var stepBackNeeded: Boolean = false

    override protected def initialState: State = onEnter {
      dispatch(Cmd("a")) to smartrobot
      wait(4 seconds)
      dispatch(Cmd("d")) to smartrobot
      wait(4 seconds)
      transit to exploring
    }

    def idle: State = onEnter {
      resetStackOfStates()
      println("Waiting for a command...")
    } onDispatch {
      case Explore(_) => transit to exploring
      case Terminate(_) => transit to terminating
      case Suspend(_) => transit to goHome
    }

    def exploring: State = stepBackIfNecessary and onEnter {
      if (beforeEmptyPos.isDefined) {
        println("Returning to beforeEmptyPos...")
        transit to returnToBeforeEmptyPos
      } else {
        lastObstacle match {
          case Some(Obstacle(_, false)) if currentBottlesCount < MAX_BOTTLE => //grab obstacle and back step, then exploring
            transit to grab

          case Some(Obstacle(_, false)) if currentBottlesCount >= MAX_BOTTLE => //back step and go to empty
            stepBackNeeded = true
            beforeEmptyPos = Some(planner.currentPosition)
            transit to emptyDetector

          case None => //explore unknown cell if possible, otherwise terminating
            planner.printMatrix()
            val plan = planner.generatePlanForExplore()
            println("Going to: " + planner.nextCellToExplore)
            if (plan.nonEmpty) {
              println("Exploring...")
              dispatch(ExecutePlan(plan)) to mySelf
              transit to executingPlan
            } else {
              transit to terminating
              println("No more to explore")
            }
        }
      }
    }

    def terminating: State = onEnter {
      if (currentBottlesCount > 0) {
        transit to emptyDetector
      } else if (planner.currentPosition != planner.robotInitialPosition) {
        transit to goHome
      } else {
        println("Terminated.")
        transit to idle
      }
    }

    def goHome: State = onEnter {
      val plan = planner.generatePlanForHome()
      if (plan.isEmpty) {
        transit to previous
      } else {
        println("Going home...")
        dispatch(ExecutePlan(plan)) to mySelf
        transit to executingPlan
      }
    }

    def emptyDetector: State = stepBackIfNecessary and onEnter {
      if (currentBottlesCount == 0) {
        transit to previous //return to caller
      } else {

        val plan = planner.generatePlanForPlasticBox()
        planner.printMatrix()
        if (plan.isEmpty) { //I'm at plstaticBox
          println("Empting...")
          request(ThrowAway(currentBottlesCount)) to plasticbox
          transit to ("wait for throwed" onReply {
            case Throwed(count) if count <= 0 =>
              println("Failed. Wait for supervisor")
              transit to ("wait for supervisor" onDispatch {
                case Continue(_) =>
                  transit to previous //wait for throwed
                  transit to previous //emptyDetector
              })
            case Throwed(count) if count > 0 =>
              transit to previous //emptyDetector
              currentBottlesCount = currentBottlesCount - count
              println("Detector is now empty")
          } timeout((4 second) -> { transit to previous }))
        } else {
          println("Going to plastixBox...")
          dispatch(ExecutePlan(plan)) to mySelf
          transit to executingPlan
        }
      }
    }

    def returnToBeforeEmptyPos: State = onEnter {
      if (beforeEmptyPos.isDefined) {
        val plan = planner.generateSafePlanFor(beforeEmptyPos.get._1, beforeEmptyPos.get._2)
        dispatch(ExecutePlan(plan)) to mySelf
        transit to executingPlan
        beforeEmptyPos = None
      } else {
        transit to previous
      }
    }

    def stepBackIfNecessary: State = onEnter {
      if (stepBackNeeded) {
        request(BackStep(lastStepFailTime - 20)) to smartrobot
        transit to ("wait back step done" onMsg {
          case StepDone(_) =>
            transit to previous
            stepBackNeeded = false
            lastObstacle = None
        })
        interruptAndTransit()
      }
    }

    def grab: State = onEnter {
      if (lastObstacle.isDefined) {
        request(Grab("x")) to grabber
        planner.setObjectAhead(Clean)
        transit to ("wait grabbed" onMsg {
          case Grabbed(success) =>
            if (success) {
              currentBottlesCount = currentBottlesCount + 1
            }
            stepBackNeeded = true
            lastObstacle = None
            transit to previous
        })
      } else {
        transit to previous
      }
    }

    def executingPlan: State = onMsg {
      case ExecutePlan(plan) if plan.isEmpty || lastObstacle.isDefined =>
        unstash()
        transit to previous

      case ExecutePlan(plan) => plan match {
        case move :: updatedPlan =>
          dispatch(ExecuteMove(move)) to mySelf
          transit to executeMove
          dispatch(ExecutePlan(updatedPlan)) to mySelf
      }

      case _ => stash()
    }

    //when transit to previous should be necessary to stepback if lastObstacle is defined
    def executeMove: State = stepBackIfNecessary and onDispatch {
      case ExecuteMove(move) if move == "w" =>
        lastObstacle = None
        request(Step(330)) to smartrobot
        wait(330 millis)
      case ExecuteMove(move) if move == "a" || move == "d" =>
        move match {
          case "a" => planner.rotateLeft()
          case "d" => planner.rotateRight()
        }
        dispatch(Cmd(move)) to smartrobot
        wait(500 millis)
        transit to previous
        unstash()
      case _ => stash()
    } onReply {
      case StepDone(_) =>
        if (lastObstacle.isEmpty) {
          planner.moveAhead()
        }
        transit to previous
        unstash()
      case StepFail(ms) =>
        lastStepFailTime = ms
        request(GetObstacleType("x")) to obstacleclassifier
      case ObstacleType(name) =>
        val obstacle = Obstacle(name, !name.contains("bottle"))
        planner.setObjectAhead(obstacle)
        lastObstacle = Some(obstacle)
        stepBackNeeded = obstacle.isStatic
        println("Found obstacle: " + name)
        transit to previous
        unstash()
    }
  }

  Detector.start()
  Detector.synchronized {
    Detector.wait()
  }

  ctxDetector.close()

}
