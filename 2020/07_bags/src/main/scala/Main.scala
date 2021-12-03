import scala.io.Source
object Main extends App {
  val file = "input"
  val lines = Source.fromFile(file).getLines.toList
  type BagContents = List[Tuple2[Int, String]]
  type BagMap = Map[String, BagContents]

  def parseBags(lines: List[String]): BagMap = {
    val bagRule = raw"(\w+ \w+) bags contain (.+)\.".r
    val numBags = raw"\s*(\d+) (\w+ \w+) bags?".r
    def parseContents(s: String): BagContents = s match {
      case "no other bags" => Nil
      case s => s.split(',').map({ case numBags(n, bag) => (n.toInt, bag)}).toList
    }
    lines.map({case bagRule(bag, bags) => bag -> parseContents(bags)}).toMap
  }

  def containsGolden(bags: Map[String, List[String]]): Map[String, Boolean] = {
    def gotGold(bag: String, visited: Map[String, Boolean]): Map[String, Boolean] = {
      if (visited contains bag)
        visited
      else {
        val vs = bags(bag).foldRight(visited)(gotGold)
        vs + (bag -> bags(bag).exists(b => b == "shiny gold" || vs(b)))
      }
    }
    bags.keys.foldRight(Map[String, Boolean]())(gotGold)
  }

  def bagsContainedIn(bag: String, bags: BagMap): Int = {
    def nIn(bag: String, visited: Map[String, Int]): Map[String, Int] = {
      if (visited contains bag)
        visited
      else {
        val vs = bags(bag).foldRight(visited)({case ((_, b), v) => nIn(b, v)})
        vs + (bag -> bags(bag).foldRight(0)({case ((n, b), a) => a + n + n * vs(b)}))
      }
    }
    nIn(bag, Map[String, Int]())(bag)
  }

  val bags = parseBags(lines)
  val bagsNoNums = bags.map({case (b, cs) => b -> cs.map(_._2)})

  println(containsGolden(bagsNoNums).count({case (_, b) => b}))
  println(bagsContainedIn("shiny gold", bags))
}
