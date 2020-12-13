import scala.io.Source

object Main extends App {
  val testShip = new Ship(Source.fromFile("test"))
  val ship = new Ship(Source.fromFile("input"))

  val manhattanDist = ((x: Int, y: Int) => x.abs + y.abs).tupled
  println(manhattanDist(testShip.last))
  println(manhattanDist(ship.last))

  val testWaypointShip = new WaypointShip(Source.fromFile("test"))
  val waypointShip = new WaypointShip(Source.fromFile("input"))
  println(manhattanDist(testWaypointShip.last))
  println(manhattanDist(waypointShip.last))
}

class Ship(source: Source) extends Iterable[Tuple2[Int, Int]] {
  val instructions = source.getLines.toList

  case class State(heading: Int, east: Int, north: Int)

  def startingState = State(0, 0, 0)

  def step(dir: String, state: State): State = state match {
    case State(h, east, north) => dir match {
      case s"N$x" => State(h, east, north + x.toInt)
      case s"S$x" => State(h, east, north - x.toInt)
      case s"E$x" => State(h, east + x.toInt, north)
      case s"W$x" => State(h, east - x.toInt, north)
      case s"F$x" => step(s"${heading(h)}$x", state)
      case s"R$x" => State((h + x.toInt) % 360, east, north)
      case s"L$x" => State((h - x.toInt + 360) % 360, east, north)
    }
  }

  def heading(h: Int): Char = h match {
    case 0 => 'E'
    case 90 => 'S'
    case 180 => 'W'
    case 270 => 'N'
    case _ => throw new Exception
  }

  def iterator: Iterator[(Int, Int)] = new ShipIterator()

  class ShipIterator extends Iterator[Tuple2[Int, Int]] {
    var insts = instructions
    var state = startingState

    def hasNext: Boolean = insts.nonEmpty

    def next(): Tuple2[Int, Int] = {
      state = step(insts.head, state)
      insts = insts.tail
      (state.east, state.north)
    }
  }
}

class WaypointShip(source: Source) extends Iterable[Tuple2[Int, Int]] {
  val instructions = source.getLines.toList
  type Point = Tuple2[Int, Int]

  def iterator = new ShipIterator()

  def rotate(p: Point, angle: Int): Point = p match {
    case (x, y) => angle match {
      case 0 => (x, y)
      case 90 => (-y, x)
      case 180 => (-x, -y)
      case 270 => (y, -x)
    }
  }

  def move(p: Point, d: Point): Point = (p, d) match {
    case ((x, y), (dx, dy)) => (x + dx, y + dy)
  }

  class ShipIterator extends Iterator[Tuple2[Int, Int]] {
    var loc = (0, 0)
    var waypoint = (10, 1)
    var insts = instructions

    def step(inst: String): Unit = inst match {
      case s"N$x" => waypoint = move(waypoint, (0, x.toInt))
      case s"S$x" => waypoint = move(waypoint, (0, -x.toInt))
      case s"E$x" => waypoint = move(waypoint, (x.toInt, 0))
      case s"W$x" => waypoint = move(waypoint, (-x.toInt, 0))
      case s"L$x" => waypoint = rotate(waypoint, x.toInt)
      case s"R$x" => waypoint = rotate(waypoint, 360 - x.toInt)
      case s"F$x" => waypoint match { case (dx, dy) =>
        loc = move(loc, (x.toInt * dx, x.toInt * dy))}
    }

    def next(): (Int, Int) = {
      step(insts.head)
      insts = insts.tail
      loc
    }

    def hasNext: Boolean = insts.nonEmpty
  }
}