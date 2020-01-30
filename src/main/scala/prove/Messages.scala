package prove

import qactor.message.Message

object Messages {

  case class Explore(nothing: String) extends Message

  case class Suspend(nothing: String) extends Message

  case class Terminate(nothing: String) extends Message

  case class Continue(nothing: String) extends Message

  //External-message
  case class Cmd(cmd: String) extends Message

  case class Step(ms: Int) extends Message

  case class BackStep(ms: Int) extends Message

  case class StepDone(nothing: String) extends Message

  case class StepFail(ms: Int) extends Message

  case class GetObstacleType(nothing: String) extends Message

  case class ObstacleType(obstacle: String) extends Message

  case class Grab(nothing: String) extends Message

  case class Grabbed(result: Boolean) extends Message

  case class ThrowAway(quantity: Int) extends Message

  case class Throwed(quantity: Int) extends Message


  //Auto-message
  case class ExecutePlan(plan: Seq[String]) extends Message

  case class ExecuteMove(move: String) extends Message

}
