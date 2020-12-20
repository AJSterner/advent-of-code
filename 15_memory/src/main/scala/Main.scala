object Main extends App {
  def step(state: Tuple2[Int, Map[Int, Int]], i: Int) = state match {
    case (x, mem) => (i - mem.getOrElse(x, i), mem.updated(x, i))
  }

  def nthFunctional(input: String, n: Int): Int = {
    val seed = input.split(',').map(_.toInt).zipWithIndex.toMap
    (seed.size to n - 2).foldLeft((0, seed))(step)._1
  }

  // Space/memory tradeoff. This is much faster
  def nth(input: String, n: Int): Int = {
    val mem = Array.fill(n)(-1)
    input.split(',').zipWithIndex.foreach(x => mem(x._1.toInt) = x._2)
    val seedSize = input.split(',').length
    (seedSize to n - 2).foldLeft(0)((x, i) => {
      val next = if (mem(x) != -1) i - mem(x) else 0
      mem(x) = i
      next
    })
  }

  println(nth("1,20,11,6,12,0", 2020))
  println(nth("1,20,11,6,12,0", 30000000))
}

