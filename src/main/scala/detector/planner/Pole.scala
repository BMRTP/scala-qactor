package detector.planner

sealed trait Pole {
  def left: Pole = this match {
    case East => North
    case North => West
    case South => East
    case West => South
  }

  def right: Pole = this.left.left.left
}

case object North extends Pole //
case object South extends Pole //
case object East extends Pole //right
case object West extends Pole //left