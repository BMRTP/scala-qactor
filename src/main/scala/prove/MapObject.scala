package prove

trait MapObject //
case class Obstacle(name: String, isStatic: Boolean) extends MapObject //
case object Unknown extends MapObject //
case object Robot extends MapObject //
case object Clean extends MapObject //