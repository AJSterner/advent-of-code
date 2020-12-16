import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Main extends App {
  println(PartOne.solution)
  println(PartTwo.solution)
}

object PartOne {
  val input = Source.fromFile("input").getLines
  val ts = input.next().toInt
  val busIds = input.next().split(',').withFilter(_ != "x").map(_.toInt)
  // could be id - (ts % id) if we know there are no ids that divide ts exactly
  def waitTime(id: Int): Int = id - (((ts - 1) % id) + 1)
  val bestId = busIds.minBy(waitTime)
  def solution = waitTime(bestId) * bestId
}

object PartTwo {
  def parseIds(s: String): List[Tuple2[Int, Int]] = {
    s.split(',')
     .zipWithIndex
     .withFilter(_._1 != "x")
     .map({case (id, idx) => (id.toInt, idx)})
     .toList
  }

  def parseIds(s: Source): List[Tuple2[Int, Int]] = {
    val lines = s.getLines
    lines.next()
    parseIds(lines.next())
  }

  @tailrec
  def gcd(a: Long, b: Long): Long = (a, b) match {
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)
  }
  def lcm(a: Long, b: Long): Long = (a / gcd(a, b)) * b

  @tailrec
  def findAligned(ts: Long, step: Long, id: Int, offset: Int): Long = {
    if ((ts + offset) % id == 0) ts else findAligned(ts + step, step, id, offset)
  }

  @tailrec
  def alignedTs(ts: Long, step: Long, ids: List[Tuple2[Int, Int]]): Long = ids match {
    case (id, offset) :: next => alignedTs(findAligned(ts, step, id, offset), lcm(step, id), next)
    case Nil => ts
  }

  def alignedTs(busIds: List[Tuple2[Int, Int]]): Long = {
    busIds.sortBy(-_._1) match { // ids in decreasing order
      case (firstId, offset) :: rest => alignedTs(firstId - offset, firstId, rest)
      case Nil => throw new Exception
    }
  }

  val busIds = parseIds(Source.fromFile("input"))
  def solution = alignedTs(busIds)
}
