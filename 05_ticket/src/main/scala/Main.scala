import scala.io.Source
import scala.collection.immutable.Nil
object Main extends App {
  val seatNumber = raw"([FB]{7})([LR]{3})".r

  def binString(one: Char)(s: String): Int =
    s.foldLeft(0)((n, c) => (n << 1) + (if (c == one) 1 else 0))
  
  val rowNum = binString('B') _
  val colNum = binString('R') _

  def seatId(seat: String): Int = seat match
    { case seatNumber(r, c) => (rowNum(r) * 8 + colNum(c)) }

  val ids = Source.fromFile("input").getLines.map(seatId).toList

  val idsSet = ids.toSet
  val mySeat = ids.find((id) => !idsSet(id + 1) && idsSet(id + 2)).get + 1

  println(ids.max)
  println(mySeat)
}
