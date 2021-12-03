import scala.io.Source
import ConwayCubes._

object Main extends App {
  println(PartOne.solution("test"))
  println(PartOne.solution("input"))
  println(PartTwo.solution("test"))
  println(PartTwo.solution("input"))
}

object ConwayCubes {
  type Vec3d = Vector[Vector[Vector[Boolean]]]

  class Cube(cube: Vec3d) {
    def mapWithIndex(f: (Boolean, Int, Int, Int) => Boolean): Vec3d = {
      cube.zipWithIndex.map({
        case (twoD, i) => twoD.zipWithIndex.map({
          case (oneD, j) => oneD.zipWithIndex.map({
            case (x, k) => f(x, i, j, k)
          })
        })
      })
    }

    def adjIdx(x: Int, y: Int, z: Int): Iterable[Tuple3[Int, Int, Int]] = {
      for {
        i <- (x - 1) to (x + 1) if cube.isDefinedAt(i)
        j <- (y - 1) to (y + 1) if cube(i).isDefinedAt(j)
        k <- (z - 1) to (z + 1) if cube(i)(j).isDefinedAt(k) && (i, j, k) != (x, y, z)
      } yield (i, j, k)
    }

    def adjActive(x: Int, y: Int, z: Int): Int =
      adjIdx(x, y, z).count({case (i, j, k) => cube(i)(j)(k)})

    def countActive(): Int = {
      cube.foldLeft(0)((a, plane) => plane.foldLeft(a)((b, r) => b + r.count(x => x)))
    }
  }
  implicit def vec3dToCube(c: Vec3d) = new Cube(c)

  type Vec4d = Vector[Vector[Vector[Vector[Boolean]]]]

  class HyperCube(hcube: Vec4d) {
    def mapWithIndex(f: (Boolean, Int, Int, Int, Int) => Boolean): Vec4d = {
      hcube.zipWithIndex.map({case (cube, i) => cube.mapWithIndex((x, j, k, l) => f(x, i, j, k, l))})
    }

    def adjIdx(w: Int, x: Int, y: Int, z: Int): Iterable[Tuple4[Int, Int, Int, Int]] = {
      for {
        i <- (w - 1) to (w + 1) if hcube.isDefinedAt(i)
        j <- (x - 1) to (x + 1) if hcube(i).isDefinedAt(j)
        k <- (y - 1) to (y + 1) if hcube(i)(j).isDefinedAt(k)
        l <- (z - 1) to (z + 1) if hcube(i)(j)(k).isDefinedAt(l) && (i, j, k, l) != (w, x, y, z)
      } yield (i, j, k, l)
    }

    def adjActive(w: Int, x: Int, y: Int, z: Int): Int =
      adjIdx(w, x, y, z).count({case (i, j, k, l) => hcube(i)(j)(k)(l)})

    def countActive(): Int = 
      hcube.foldLeft(0)((a, cube) => a + cube.countActive())
  }
  implicit def vec4dToCube(hc: Vec4d) = new HyperCube(hc)

  def printCube(cube: Vec3d): Unit = {
    cube.foreach(p => {p.foreach(x => println(x.map(y => if (y) '#' else '.').mkString)); println()})
  }

  def conwayRule(active: Boolean, adj: Int): Boolean = (adj == 3) || (active && adj == 2)

  def step3d(cube: Vec3d): Vec3d = {
    cube.mapWithIndex((x, i, j, k) => conwayRule(x, cube.adjActive(i, j, k)))
  }

  def step4d(hcube: Vec4d): Vec4d = {
    hcube.mapWithIndex((active, i, j, k, l) => conwayRule(active, hcube.adjActive(i, j, k, l)))
  }
}

object PartOne {
  def surround[A, B >: A](core: Vector[A],  padding: Iterable[B]): Vector[B] = padding ++: core :++ padding
  def parseCube(filename: String, turns: Int): Vec3d = {
    val lines = Source.fromFile(filename).getLines.toVector
    val initPlane = lines.map(_.map(_ == '#').toVector)
    val p1d = initPlane.map(r => surround(r, Vector.fill(turns)(false)))
    val p2d = surround(p1d, Vector.fill(turns, p1d(0).size)(false))
    surround(Vector(p2d), Vector.fill(turns, p2d.size, p2d(0).size)(false))
  }

  def solution(filename: String): Int = {
    val start = parseCube(filename, 6)
    (0 until 6).foldLeft(start)((cube, _) => step3d(cube)).countActive()
  }
}

object PartTwo {
  def parseHyperCube(filename: String, turns: Int): Vec4d = {
    val cube = PartOne.parseCube(filename, turns)
    PartOne.surround(Vector(cube), Vector.fill(turns, cube.size, cube(0).size, cube(0)(0).size)(false))
  }

  def solution(filename: String): Int = {
    val start = parseHyperCube(filename, 6)
    (0 until 6).foldLeft(start)((hcube, _) => step4d(hcube)).countActive()
  }
}