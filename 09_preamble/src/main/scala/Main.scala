import scala.collection.mutable.HashMap
import scala.io.Source
import scala.collection.immutable.Nil
object Main extends App {
  def parseData(file: String) = Source.fromFile(file).getLines().toList.map(_.toLong)
  val testData = parseData("test")
  val data = parseData("input")

  val testP = new Preamble(testData, 5)
  val testInvalid = testP.find({case (n, sums) => !sums(n)}).map(_._1).get
  println(testInvalid)
  val p = new Preamble(data, 25)
  val invalid = p.find({case (n, sums) => !sums(n)}).map(_._1).get
  println(invalid)

  val testContig = ContiguousSum.conSum(testData, testInvalid)
  println(testContig.min + testContig.max)

  val contig = ContiguousSum.conSum(data, invalid)
  println(contig.min + contig.max)
}

class Preamble(data: List[Long], len: Int) extends Iterable[Tuple2[Long, Set[Long]]] {

  def iterator: Iterator[(Long, Set[Long])] = new PreambleIterator

  class PreambleIterator extends Iterator[Tuple2[Long, Set[Long]]] {
    var sums = HashMap[Long, Long]()
    type Acc = List[Tuple2[Long, List[Long]]]
    var acc: Acc = Nil

    data.take(len).foreach(pushToAcc)
    var remaining = data.drop(len)

    def dropFromSums(x: Long) = {
      sums.updateWith(x)(_.map(_ - 1).filter(_ != 0))
      ()
    }
    
    def addToSums(x: Long) = {
      sums.updateWith(x)(_.map(_ + 1) orElse Some(1))
      ()
    }
    
    def pushToAcc(n: Long) = {
      def pushBack(a: Acc): Acc = a match {
        case (m, s) :: xs => {
          addToSums(n + m)
          (m -> ((n + m) :: s)) :: pushBack(xs)
        }
        case Nil => (n -> Nil) :: Nil
      }
      acc = pushBack(acc)
    }

    def popFromAcc() = acc match {
      case (_, s) :: xs => {
        s.foreach(dropFromSums)
        acc = xs
      }
      case Nil => ()
    }

    def hasNext: Boolean = remaining match {
      case _ :: Nil => false
      case Nil => false
      case _ :: _ => true
    }

    def next(): (Long, Set[Long]) = {
      popFromAcc()
      pushToAcc(remaining.head)
      remaining = remaining.tail
      (remaining.head, sums.keys.toSet)
    }
  }
}

object ContiguousSum {
  def conSumList(data: List[Long], n: Long): Option[List[Long]] = data match {
    case x :: xs =>
      if (x == n)
        Some(x :: Nil)
      else if (x < n)
        conSumList(xs, n - x).map(x :: _)
      else
        None
    case Nil => None
  }

  def conSum(data: List[Long], n: Long): List[Long] = data match {
    case _ :: xs => conSumList(data, n).filter(_.lengthCompare(1) > 0) getOrElse conSum(xs, n)
    case Nil => Nil
  } 
}