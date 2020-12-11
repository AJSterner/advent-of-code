import scala.io.Source

object Main extends App {
  def twoSumProduct(xs: List[Int], k: Int) : Option[Int] = {
    val inXs = xs.toSet
    xs.find(inXs(k - _)).map { _ * (k - _)}
  }

  def threeSumProduct(xs: List[Int], k: Int) : Option[Int] = xs match {
    case x :: xs =>
      twoSumProduct(next, k - head) map {_ * x} orElse threeSumProduct(next, k)
    case Nil => None
  }

  def nSumProduct(xs: List[Int], k: Int, n: Int) : Option[Int] = {
    def twoSum(xs: List[Int], ys: List[Int], k: Int): Option[Int] = (xs, ys) match {
      case (x :: xs, y :: ys) =>
        if (x + y == k) 
          Some(x * y)
        else if (x + y < k)
          twoSum(xs, y :: ys, k)
        else 
          twoSum(x :: xs, ys, k)
      case (_, _) => None
    }
    def sumProduct(xs: List[Int], k: Int, n: Int): Option[Int] = (xs, n) match {
      case (_, 0) => None
      case (xs, 1) => xs.find(_ == k)
      case (xs, 2) => twoSum(xs, xs.reverse, k)
      case (x :: xs, n) =>
        sumProduct(xs, k - x, n - 1) map { _ * x } orElse sumProduct(xs, k, n)
      case (Nil, _) => None
    }
    sumProduct(xs.sorted, k, n)
  }

  val nums = Source.fromFile("input").getLines.map(_.toInt).toList
  println(twoSumProduct(nums, 2020))
  println(nSumProduct(nums, 2020, 2))
  println(threeSumProduct(nums, 2020))
  println(nSumProduct(nums, 2020, 3))
}
