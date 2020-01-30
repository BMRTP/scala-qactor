package prove

import utils.ExtensionMethods._

abstract class MatrixPlanner(robotInitialPosition: ((Int, Int), Pole)) {

  private val map: scala.collection.mutable.Map[(Int, Int), MapObject] = scala.collection.mutable.Map()

  private var direction: Pole = robotInitialPosition._2

  map.addOne(robotInitialPosition._1, Robot)

  def currentPosition: ((Int, Int), Pole) = (map.find {
    case (_, Robot) => true
    case _ => false
  }.head._1, direction)

  def setPosition(coordinate: (Int, Int), direction: Pole): Unit = {
    setMapAt(currentPosition._1, Clean)
    setMapAt(coordinate, Robot)
    this.direction = direction
  }

  def setMapAt(position: (Int, Int), obj: MapObject): Unit = map.addOne((position, obj))

  def getMapAt(position: (Int, Int)): MapObject = if (map.isDefinedAt(position)) {
    map(position)
  } else {
    Unknown
  }

  def nextCellToExplore: Option[(Int, Int)]


  def findPath(source: (Int, Int), target: (Int, Int), validCell: MapObject => Boolean, l: Int, maxL: Int): Seq[(Int, Int)] = {
    if (maxL == l) {
      Seq()
    } else if (source == target) {
      Seq(source)
    } else {
      val path = source.neighbors.filter(p => validCell(getMapAt(p))).iterator.map(p => findPath(p, target, validCell, l + 1, maxL))
        .filter(_.nonEmpty).toSeq.sortBy(_.size).headOption.getOrElse(Seq())
      if (path.nonEmpty) {
        Seq(source) ++ path
      } else {
        Seq()
      }
    }
  }

  def getRotationPlan(currentDirection: Pole, targetDirection: Pole): Seq[String] =
    currentDirection match {
      case North => targetDirection match {
        case North => Seq()
        case South => Seq("a", "a")
        case East => Seq("d")
        case West => Seq("a")
      }
      case South => targetDirection match {
        case North => Seq("a", "a")
        case South => Seq()
        case East => Seq("a")
        case West => Seq("d")
      }
      case East => targetDirection match {
        case North => Seq("a")
        case South => Seq("d")
        case East => Seq()
        case West => Seq("a", "a")
      }
      case West => targetDirection match {
        case North => Seq("d")
        case South => Seq("a")
        case East => Seq("a", "a")
        case West => Seq()
      }
    }

  def getRotationPlan(source: (Int, Int), target: (Int, Int), currentDirection: Pole): (Seq[String], Pole) = {
    val targetDirection = source match {
      case (x, y) => target match {
        case (x1, y1) if x1 == x && y1 < y => North
        case (x1, y1) if x1 == x && y1 > y => South
        case (x1, y1) if x1 < x && y1 == y => West
        case (x1, y1) if x1 > x && y1 == y => East
      }
    }
    (getRotationPlan(currentDirection, targetDirection), targetDirection)
  }

  def pathToPlan(path: Seq[(Int, Int)]): (Seq[String], Pole) = path match {
    case s if s.isEmpty => (Seq(), currentPosition._2)
    case s =>
      val res = s.drop(1).foldLeft((currentPosition, Seq[String]())) {
        case (data, p) =>
          val result = getRotationPlan(data._1._1, p, data._1._2)
          val ret = ((p, result._2), data._2 ++ result._1 ++ Seq("w"))
          ret
      }
      (res._2, res._1._2)
  }

  def generatePlanForTerminate(): Seq[String] = generateSafePlanFor(robotInitialPosition._1, robotInitialPosition._2)

  def generatePlanForPlasticBox(): Seq[String] = generatePlanForTerminate()


  def generatePlanForExplore(): Seq[String] =
    nextCellToExplore match {
      case Some(target) =>
        val path = findPath(currentPosition._1, target, {
          case Unknown | Clean | Obstacle(_, false) => true
          case _ => false
        }, 0, 10)
        pathToPlan(path)._1
      case None => Seq()
    }

  def generateSafePlanFor(pos: (Int, Int), direction: Pole): Seq[String] = {
    val path = findPath(currentPosition._1, pos, v => v == Clean, 0, 15)
    val plan = pathToPlan(path)
    plan._1 ++ getRotationPlan(plan._2, direction)
  }

  def setObjectAhead(obj: MapObject): Unit = setMapAt(aheadPosition, obj)

  def aheadPosition: (Int, Int) = currentPosition match {
    case ((x, y), rot) => rot match {
      case East => (x + 1, y)
      case North => (x, y - 1)
      case South => (x, y + 1)
      case West => (x - 1, y)
    }
  }

  def moveAhead(): Unit = setPosition(aheadPosition, currentPosition._2)

  def rotateLeft(): Unit = currentPosition match {
    case (pos, rot) => setPosition(pos, rot.left)
  }

  def rotateRight(): Unit = currentPosition match {
    case (pos, rot) => setPosition(pos, rot.right)
  }

  def printMatrix(): Unit = {
    val rows = map.map(_._1._2).max
    val columns = map.map(_._1._1).max
    var r = 0
    var c = 0
    println("MAP:")
    while (r <= rows) {
      c = 0
      while (c <= columns) {
        val v = getMapAt((c, r))
        val char = v match {
          case Clean => "0"
          case Obstacle(name, true) => "X"
          case Obstacle(name, false) => "B"
          case Robot => "r"
          case Unknown => " "
        }
        print(char)
        c = c + 1
      }
      r = r + 1
      print("\n")
    }
  }
}

trait DummyMatrixPlanner {
  self: MatrixPlanner =>

  override def nextCellToExplore: Option[(Int, Int)] = {

    def nextCell(origin: (Int, Int)): Option[(Int, Int)] = {

      def filter(ns: Seq[(Int, Int)]): Option[(Int, Int)] = ns.flatMap(n => n.neighbors).find(p => getMapAt(p) match {
        case Unknown | Obstacle(_, false) => true
        case _ => false
      })

      def filterNs(p: (Int, Int)): Boolean = getMapAt(p) match {
        case Clean | Robot => true
        case _ => false
      }

      var ns = origin +: origin.neighbors.filter(filterNs)
      var i = 0
      while (filter(ns).isEmpty && i < 20) {
        ns = ns.flatMap(n => n.neighbors).distinct.filter(filterNs)
        i = i + 1
      }
      filter(ns)

    }

    nextCell(currentPosition._1)
  }
}