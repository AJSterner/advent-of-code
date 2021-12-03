import scala.io.Source
import scala.util.matching.Regex
object Main extends App {
  val passPattern : Regex = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$".r

  def countValid(p: (Int, Int, Char, String) => Boolean): Int = {
    Source.fromFile("input").getLines.count({
      case passPattern(low, high, char, password) => p(low.toInt, high.toInt, char.charAt(0), password)
      case _ => false
    })
  }

  def charsInRange(low: Int, high: Int, c: Char, pass: String) = {
    val count = pass.count(_ == c)
    low <= count && count <= high
  }

  def charsAtPosition(i: Int, j: Int, c: Char, pass: String) = {
    (i - 1 < pass.length && pass.charAt(i - 1) == c) ^ (j - 1 < pass.length && pass.charAt(j - 1) == c)
  }

  println(countValid(charsInRange))
  println(countValid(charsAtPosition))
}
