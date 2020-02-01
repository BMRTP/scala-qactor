package prove

import utils.ExtensionMethods._

abstract class MatrixPlanner(val robotInitialPosition: ((Int, Int), Pole), val robotEmptyPosition: ((Int, Int), Pole)) {

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


  def getPlan(source: ((Int, Int), Pole), target: ((Int, Int), Pole), validCell: MapObject => Boolean): Option[Seq[String]] = {

    def getRotationPlan(currentDirection: Pole, targetDirection: Pole): Seq[String] = currentDirection match {
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

    def getRotationPlan2(source: (Int, Int), target: (Int, Int), currentDirection: Pole): (Seq[String], Pole) = {
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

    def findPaths(source: (Int, Int), target: (Int, Int), validCell: MapObject => Boolean): Seq[Seq[(Int, Int)]] = {
      if (source == target) {
        Seq(Seq())
      } else {
        val indexMap: scala.collection.mutable.Map[(Int, Int), Int] = scala.collection.mutable.Map()
        indexMap.addOne(source, 0)
        var found = false
        var index = 0
        while (!found) {
          val possibleCells = indexMap.toSeq.collect({
            case (tuple, i) if i == index => tuple
          }).flatMap(_.neighbors).distinct.filter(p => validCell(getMapAt(p)) && !indexMap.isDefinedAt(p))
          if (possibleCells.contains(target)) {
            indexMap.addOne(target, index + 1)
            found = true
          } else {
            possibleCells.foreach(p => {
              indexMap.addOne(p, index + 1)
            })
            index = index + 1
          }
        }

        def getPaths(indexMap: Map[(Int, Int), Int], index: Int, myPos:(Int,Int)): Seq[Seq[(Int, Int)]] = {
          if (!indexMap.exists(_._2 == index)) {
            Seq(Seq())
          } else {
            for ((pos, _) <- indexMap.filter(p => p._2 == index && (p._1.neighbors.contains(myPos) || myPos == p._1)).toSeq;
                 path <- getPaths(indexMap, index + 1, pos)) yield pos +: path
          }
        }

        getPaths(indexMap.toMap, 0, source)
      }
    }

    def pathToPlan(path: Seq[(Int, Int)], sourceRotation: Pole, destRotation: Pole): Seq[String] = path match {
      case s if s.isEmpty => getRotationPlan(sourceRotation, destRotation)
      case s =>
        s.drop(1).foldLeft(((path.head, sourceRotation), Seq[String]())) {
          case (((currentCell, currentRotation), partialPlan), targetCell) =>
            getRotationPlan2(currentCell, targetCell, currentRotation) match {
              case (res, newRotation) => ((targetCell, newRotation), partialPlan ++ res ++ Seq("w"))
            }
        } match {
          case ((_, currentRotation), plan) => plan ++ getRotationPlan(currentRotation, destRotation)
        }
    }

    findPaths(source._1, target._1, validCell).map(path => {
      pathToPlan(path, source._2, target._2)
    }).sortBy(_.size).headOption
  }

  def generatePlanForHome(): Seq[String] = generateSafePlanFor(robotInitialPosition)

  def generatePlanForPlasticBox(): Seq[String] = generateSafePlanFor(robotEmptyPosition)

  def generatePlanForExplore(): Seq[String] =
    nextCellToExplore match {
      case Some(target) => getPlan(currentPosition, (target, North), {
        case Unknown | Clean | Obstacle(_, false) => true
        case _ => false
      }).getOrElse(Seq()).reverse.dropWhile(m => m == "a" || m == "d").reverse //drop last rotation if present
      case None => Seq()
    }

  def generateSafePlanFor(target: ((Int, Int), Pole)): Seq[String] =
    getPlan(currentPosition, target, {
      case Clean => true
      case _ => false
    }).getOrElse(getPlan(currentPosition, target, {
      case Clean | Unknown => true
      case _ => false
    }).getOrElse(Seq()))

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

  def mapString: String = {
    val rows = map.map(_._1._2).max
    val columns = map.map(_._1._1).max
    var r = map.map(_._1._2).min
    var c = map.map(_._1._1).min
    val stringBuilder = new StringBuilder()
    while (r <= rows) {
      c = map.map(_._1._1).min
      while (c <= columns) {
        val v = getMapAt((c, r))
        val char = v match {
          case Clean => '0'
          case Obstacle(name, true) => 'X'
          case Obstacle(name, false) => 'B'
          case Robot => 'r'
          case Unknown => ' '
        }
        stringBuilder.addOne(char)
        c = c + 1
      }
      r = r + 1
      stringBuilder.addOne('\n')
    }
    stringBuilder.toString
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