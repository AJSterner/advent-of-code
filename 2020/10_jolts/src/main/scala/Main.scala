import scala.io.Source
import scala.collection.mutable.HashMap
object Main extends App {
  def parseData(file: String) = Source.fromFile(file).getLines.toList.map(_.toInt)
  val testData = parseData("test")
  val data = parseData("input")

  def joltDiffs(data: List[Int]): Tuple3[Int, Int, Int] = {
    data.sorted.sliding(2).foldRight((0, 0, 0)) {
      case ((x :: y :: Nil), (ones, twos, threes)) => (y - x) match {
        case 1 => (ones + 1, twos, threes)
        case 2 => (ones, twos + 1, threes)
        case 3 => (ones, twos, threes + 1)
      }
    }
  }

  val testJoltDiffMult = joltDiffs(0 :: (testData.max + 3) :: testData) match {case (x, _, z) => x * z}
  println(testJoltDiffMult)
  val joltDiffMult = joltDiffs(0 :: (data.max + 3) :: data) match {case (x, _, z) => x * z}
  println(joltDiffMult)

  def joltCombinations(converters: Iterable[Int], target: Int): BigInt = {
    val cSet = converters.toSet.incl(target)
    val comboHash = new DefaultHashMap[Int, BigInt]((k, h) => 
      if (k == 0)
        1
      else if (k < 0 || !cSet(k))
        0
      else 
        h(k - 1) + h(k - 2) + h(k - 3)
    )
    comboHash(target)
  }

  println(joltCombinations(testData, testData.max + 3))
  println(joltCombinations(data, data.max + 3))
}

class DefaultHashMap[K, V](defaultFunc: (K, DefaultHashMap[K, V]) => V) extends scala.collection.mutable.HashMap[K, V] {
  override def default(key: K): V = {
    val value = defaultFunc(key, this)
    put(key, value)
    value
  }
}
