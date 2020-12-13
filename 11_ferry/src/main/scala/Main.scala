import scala.io.Source
import scala.collection.IndexedSeqView
import Ferry.{Ferry, SightLineFerry, printSeats}
object Main extends App {
  val testFerry = new Ferry(Source.fromFile("test"))
  val ferry = new Ferry(Source.fromFile("input"))

  println(Ferry.occupiedSeats(testFerry.last))
  println(Ferry.occupiedSeats(ferry.last))
  
  val testSightFerry = new SightLineFerry(Source.fromFile("test"))
  val sightFerry = new SightLineFerry(Source.fromFile("input"))
  println(Ferry.occupiedSeats((testSightFerry.last)))
  println(Ferry.occupiedSeats((sightFerry.last)))
}

object Ferry {
  type Seats = Array[Array[Char]]
  def printSeats(ferry: Seats): Unit = {
    ferry.tapEach(x => println(x.mkString))
    println()
  }

  def toView(seats: Seats): Iterable[Iterable[Char]] = seats.map(_.view).view
  
  def occupiedSeats(area: Iterable[Iterable[Char]]): Int = 
    area.foldRight(0)((row, a) => a + row.count(_ == '#'))
  
  def occupiedSeats(area: Seats): Int = occupiedSeats(toView(area))
  
  class Ferry(source: Source) extends Iterable[Seats] {
    var seats: Seats = source.getLines.toArray.map(_.toCharArray)
    def step(ferry: Seats): Seats = {
      ferry.zipWithIndex.map {case (row, r) => 
        row.indices.map(step(ferry, r, _)).toArray
      }
    }

    def step(ferry: Seats, r: Int, c: Int): Char = {
      ferry(r)(c) match {
        case '.' => '.'
        case 'L' => if (areaOccupiedSeats(ferry, r, c) == 0) '#' else 'L'
        case '#' => if (areaOccupiedSeats(ferry, r, c) >= 5) 'L' else '#'
        case _ => throw new Exception
      }
    }

    def surrounding(ferry: Seats, r: Int, c: Int): IndexedSeqView[IndexedSeqView[Char]] = 
      ferry.view.slice(r - 1, r + 2).map(_.view.slice(c - 1, c + 2))
    
    def areaOccupiedSeats(ferry: Seats, r: Int, c: Int): Int = 
      occupiedSeats(surrounding(ferry, r, c))

    def iterator: Iterator[Seats] = new FerryIterator

    class FerryIterator extends Iterator[Seats] {
      var prev: Seats = seats.map(_.map(_ => 'x'))
      var curr = seats.clone()
      def hasNext: Boolean =
        prev.lazyZip(curr).exists((x, y) => !x.sameElements(y))
      
      def next(): Seats = {
        prev = curr
        curr = step(prev)
        prev
      }
    }
  }

  class SightLineFerry(source: Source) extends Ferry(source) {
    override def step(ferry: Seats): Seats = {
      val counts = seatCounts(ferry)
      ferry.zipWithIndex.map {case (row, r) => 
        row.indices.map(step(ferry, counts, r, _)).toArray
      }
    }

    def step(ferry: Seats, counts: Array[Array[Int]], r: Int, c: Int) = {
      ferry(r)(c) match {
        case '.' => '.'
        case 'L' => if (counts(r)(c) == 0) '#' else 'L'
        case '#' => if (counts(r)(c) >= 5) 'L' else '#'
        case _ => throw new Exception
      }
    }

    def seatCounts(ferry: Seats): Array[Array[Int]] = {
      val counts = ferry.map(_.map(_ => 0))

      def sightLine(inSight: Char, curr: Char): Char =
        if (curr == '.') inSight else curr

      // adds one to counts if filled seat in sight. Returns the next item in sight
      def countInSight(inSight: Char, curr: Char, row: Int, col: Int): Char = {
        if (inSight == '#') counts(row)(col) += 1
        sightLine(inSight, curr)
      }

      // iterates over each line of sight and calls countInSight
      // func m maps loc in line of sight to loc in ferry
      def countDir(lines: Iterable[Iterable[Char]], m: (Int, Int) => (Int, Int)): Unit = {
        lines.zipWithIndex.foreach({case (line, x) =>
          line.zipWithIndex.foldRight('.')({case ((curr, y), inSight) =>
            m(x, y) match {case (row, col) => countInSight(inSight, curr, row, col)}
          })
          line.zipWithIndex.foldLeft('.')({case (inSight, (curr, y)) =>
            m(x, y) match {case (row, col) => countInSight(inSight, curr, row, col)}
          })
        })
      }
      val rowLines = toView(ferry)
      val colLines = toView(ferry.transpose)
      // lines going up right following the edge from up left to bottom left to bottom right
      val upRight: Array[List[Char]] = Array.fill(ferry.size + ferry(0).size - 1)(Nil)
      // lines going up left following the edge from up right to bottom right to bottom left
      val upLeft: Array[List[Char]] = Array.fill(ferry.size + ferry(0).size - 1)(Nil)
      val w = ferry(0).size
      val h = ferry.size
      for ((row, r) <- ferry.zipWithIndex; (curr, c) <- row.zipWithIndex) {
        upRight(r + c) = curr :: upRight(r + c)
        upLeft(w - c - 1 + r) = curr :: upLeft(w - c - 1 + r)
      }

      countDir(rowLines, (x, y) => (x, y))
      countDir(colLines, (x, y) => (y, x))
      def j(x: Int, y: Int) = y + (0 max (x - h + 1))
      countDir(upRight, (x, y) => (x - j(x, y), j(x, y)))
      countDir(upLeft,  (x, y) => (x - j(x, y), w - j(x, y) - 1))
      counts
    }
  }
}
