import scala.io.Source
object Main extends App {
  def countTrees(lines: List[String], right: Int, down: Int): Int = 
    lines.view.zipWithIndex.count({ case (treeline, i) =>
      (i % down == 0) && (treeline.charAt((i / down * right) % treeline.length) == '#')
    })
  
  val lines = Source.fromFile("input").getLines.toList
  println(countTrees(lines, 3, 1))

  val slopes: List[Tuple2[Int, Int]] = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  println(slopes.map({ case (r, d) => countTrees(lines, r, d)}).foldRight(1)(_*_))
}
