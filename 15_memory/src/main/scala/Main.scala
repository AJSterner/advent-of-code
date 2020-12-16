object Main extends App {
  def step(state: Tuple2[Int, Map[Int, Int]], i: Int) = state match {
    case (x, mem) => (i - mem.getOrElse(x, i), mem.updated(x, i))
  }
  def nth(input: String, n: Int): Int = {
    val seed = input.split(',').map(_.toInt).zipWithIndex.toMap
    (seed.size to n - 2).foldLeft((0, seed))(step)._1
  }

  println(nth("1,20,11,6,12,0", 2020))
  println(nth("1,20,11,6,12,0", 30000000))
}
