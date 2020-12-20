import scala.io.Source
import Shared._

object Main extends App {
  println(PartOne.solution("test"))
  println(PartOne.solution("input"))
  println(PartTwo.solution("test2"))
  println(PartTwo.solution("input"))
}

object Shared {
  type InclRange = Tuple2[Int, Int]
  type FieldRange = Tuple2[String, Tuple2[InclRange, InclRange]]
  def splitOnEmpty(lines: List[String]): List[List[String]] = lines.span(!_.isEmpty()) match {
    case (xs, _ :: ys) => xs :: splitOnEmpty(ys)
    case (xs, Nil) => xs :: Nil
  }
  def parseInput(lines: List[String]) = {
    val range = raw"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)".r
    def parseRanges(input: List[String]): List[FieldRange] = input match {
      case range(nm, r0, r1, r2, r3) :: ls => (nm, ((r0.toInt, r1.toInt), (r2.toInt, r3.toInt))) :: parseRanges(ls)
      case Nil => Nil
    }
    def parseTicket(line: String): List[Int] = line.split(',').map(_.toInt).toList
    def parseTickets(lines: List[String]): List[List[Int]] = lines.map(parseTicket)

    splitOnEmpty(lines) match {
      case ranges :: ("your ticket:" :: myTicket :: Nil) :: ("nearby tickets:" :: tix) :: Nil =>
        (parseRanges(ranges), parseTicket(myTicket), parseTickets(tix))
      case _ => throw new Exception
    }
  }
  def bt(x: Int)(range: InclRange): Boolean = range._1 <= x && x <= range._2
  def inRange(x: Int)(range: FieldRange) = bt(x)(range._2._1) || bt(x)(range._2._2)
  def validField(ranges: List[FieldRange])(x: Int): Boolean = ranges.exists(inRange(x))
}

object PartOne {
  def solution(file: String): Int = {
    val (ranges, myTicket, tix) = parseInput(Source.fromFile(file).getLines().toList)
    val invalid = tix.flatMap(_.filterNot(validField(ranges)))
    invalid.sum
  }
}

object PartTwo {
  def restrictFields(possibles: List[List[FieldRange]], t: List[Int]): List[List[FieldRange]] = {
    possibles.zip(t).map({case (rs, x) => rs.filter(inRange(x))})
  }

  def solution(file: String): Long = {
    val (ranges, myTicket, tix) = parseInput(Source.fromFile(file).getLines().toList)
    val validTix = tix.filter(_.forall(validField(ranges)))
    val possibles = List.fill(myTicket.length)(ranges)
    val restricted = (myTicket :: validTix).foldLeft(possibles)(restrictFields)
    val fieldOrdersList = restricted.zipWithIndex.sortBy(_._1.length).foldLeft((Set[FieldRange](), List[Tuple2[List[FieldRange], Int]]()))({
      case ((seen, xs), (fs, i)) => (seen ++ fs, ((fs.filterNot(seen), i) :: xs))
    })._2
    assert(fieldOrdersList.forall(_._1.length == 1))

    val fieldOrders = fieldOrdersList.sortBy(_._2).map(_._1.head._1)

    myTicket.zip(fieldOrders).foldLeft(1L)({
      case (a, (x, f)) => if (f.startsWith("departure")) a * x else a
    })
  }
}