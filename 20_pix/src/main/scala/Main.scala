import scala.io.Source
import Pix.{pt1, pt2}

object Main extends App {
  val lines = Source.fromFile("input").getLines().toVector
  println(pt1(lines))
  println(pt2(lines))
}

class Tile(val id: Int, val img: Vector[Vector[Char]]) {
  def top() = img.head
  def bottom() = img.last.reverse
  def left() = img.map(_.head).reverse
  def right() = img.map(_.last)

  def edge(d: Int): Vector[Char] = d match {
    case 0 => top()
    case 90 => right()
    case 180 => bottom()
    case 270 => left()
  }

  def edges() = Vector(top(), right(), bottom(), left())
  def allEdges() = edges() :++ edges().map(_.reverse)

  def body() = img.tail.dropRight(1).map(_.tail.dropRight(1))

  def rotate(d: Int): Tile = (((d % 360) + 360) % 360) match {
    case 0 => new Tile(id, img)
    case 90 => new Tile(id, img.transpose.map(_.reverse))
    case 180 => new Tile(id, img.reverse.map(_.reverse))
    case 270 => new Tile(id, img.transpose.reverse)
  }

  def flipH(): Tile = new Tile(id, img.map(_.reverse))
  def flipV(): Tile = new Tile(id, img.reverse)

  def rotations() = (0 to 270 by 90).map(rotate)
  def allRotations() = rotations() ++ flipV().rotations()

  def gs() = img.flatMap(_.map(_ == '#'))
  def width = img(0).size
  def height = img.size

  override def clone(): Tile = new Tile(id, img)
  override def toString(): String = img.map(_.mkString :+ '\n').mkString
}

object Pix {
  def splitOnEmpty(lines: Vector[String]): Vector[Vector[String]] = {
    lines.dropWhile(_.isEmpty()).span(_.nonEmpty) match {
      case (xs, "" +: ys) => xs +: splitOnEmpty(ys)
      case (xs, _) if xs.nonEmpty => Vector(xs)
      case (xs, _) => Vector()
    }
  }

  def matchEdges(tiles: Vector[Tile]): Map[String, Vector[Tile]] = {
    tiles.foldLeft(Map[String, Vector[Tile]]())((m, t) => 
      t.allEdges().foldLeft(m)((m, e) =>
        m.updatedWith(e.mkString)(ids =>
          ids map { _.appended(t) } orElse Option(Vector(t))
        )
      )
    )
  }

  def parseTile(lines: Vector[String]): Tile = lines match {
    case s"Tile $id:" +: rs => new Tile(id.toInt, rs.map(_.toVector))
  }

  def parseTiles(lines: Vector[String]) = splitOnEmpty(lines).map(parseTile)

  def corners(tiles: Vector[Tile], matched: Map[String, Vector[Tile]]): Vector[Tile] = 
    tiles.filter(t => t.edges().count(e => matched(e.mkString).length == 1) == 2)

  def pt1(lines: Vector[String]): Long = {
    val tiles = parseTiles(lines)
    val cs = corners(tiles, matchEdges(tiles))
    assert(cs.size == 4)
    cs.foldLeft(1L)((a, t) => a * t.id)
  }

  def buildPicture(tiles: Vector[Tile]): Tile = {
    val matched = matchEdges(tiles)

    def toTL(corner: Tile) = {
      if (matched(corner.top().mkString).size == 1)
        if (matched(corner.left().mkString).size == 1)
          corner.clone()
        else
          corner.flipH()
      else
        if (matched(corner.left().mkString).size == 1)
          corner.flipV()
        else
          corner.flipV().flipH()
    }

    def neighbor(t: Tile, d: Int): Option[Tile] = 
      matched(t.edge(d).mkString) find {_.id != t.id} map (t1 => matchEdge(t, t1, d))

    def matchEdge(t: Tile, t1: Tile, d: Int): Tile = {
      t1.edges().indexOf(t.edge(d).reverse) match {
        case -1 => matchEdge(t, t1.flipV(), d)
        case i => t1.rotate(d + 180 - (i * 90))
      }
    }

    def buildRow(t: Option[Tile]): Vector[Tile] = 
      t map (t => t +: buildRow(neighbor(t, 90))) getOrElse Vector()

    def build(t: Option[Tile]): Vector[Vector[Tile]] = buildRow(t) match {
      case t +: ts => (t +: ts) +: build(neighbor(t, 180))
      case _ => Vector()
    }

    def mergeRow(r: Vector[Tile]): Vector[Vector[Char]] = 
      r.map(_.body()).reduceRight((b, a) => b.zip(a) map {case (xs, ys) => xs ++: ys})
    
    def mergeTiles(rows: Vector[Vector[Tile]]): Vector[Vector[Char]] = rows.flatMap(mergeRow)

    val tL = toTL(corners(tiles, matched).head)

    new Tile(0, mergeTiles(build(Option(tL))))
  }

  def pic(lines: Vector[String]): String = buildPicture(parseTiles(lines)).toString()

  val monster = Vector("                  # ",
                       "#    ##    ##    ###",
                       " #  #  #  #  #  #   ")
  def monsterAligned(t: Tile): Vector[Boolean] = 
    monster.mkString(" " * (t.width - monster(0).size)).map(_ == '#').toVector

  def countMonsters(t: Tile) = {
    val ms = monsterAligned(t)
    t.gs().sliding(ms.size).count(
      _.zip(ms).forall {
        case (p, m) => (m && p) == m
      }
    )
  }

  def countMonstersAllRotations(t: Tile) = 
    t.allRotations().map(countMonsters)
  
  def roughness(t: Tile) = 
    t.gs().count(x => x) - (monsterAligned(t).count(x => x) * countMonstersAllRotations(t).max)

  def pt2(lines: Vector[String]): Long = 
    roughness(buildPicture(parseTiles(lines)))
}