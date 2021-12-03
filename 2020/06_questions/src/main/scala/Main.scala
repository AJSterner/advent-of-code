import scala.io.Source
import scala.collection.immutable.Set

object Main extends App {
  val file = "input"
  val lines = Source.fromFile(file).getLines.toList

  def parseGroups(lines: List[String]): List[List[Set[Char]]] = {
    def group(lines: List[String]): List[Set[Char]] =
      lines map { _.toSet }
    
    lines.span(_.nonEmpty) match {
      case (xs, _ :: ys) => group(xs) :: parseGroups(ys)
      case (xs, Nil) => group(xs) :: Nil
    }
  }

  val groups = parseGroups(lines)
  val groupSumsAny = groups map {gs => gs.foldLeft(gs.head)((g, s) => g union s).size}
  val groupSumsAll = groups map {gs => gs.foldLeft(gs.head)((g, s) => g intersect s).size}
  println(groupSumsAny.sum)
  println(groupSumsAll.sum)
}
