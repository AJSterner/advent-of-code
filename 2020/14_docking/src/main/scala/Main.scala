// refactoring inspired by:
// https://github.com/haskelling/aoc2020/blob/main/14b.hs
import scala.io.Source
import scala.collection.immutable.HashMap

object Main extends App {
  println(PartOne.solution("input"))
  println(PartTwo.solution("input"))
}

object PartOne {
  type Mask = Tuple2[Long, Long]
  type Mem = HashMap[Long, Long]
  type State = Tuple2[Mask, Mem]
  def initState: State = ((0, 0), HashMap[Long, Long]())

  def exec(state: State, line: String): State = (state, line) match {
    case ((_, mem),    s"mask = $s") => (parseMask(s), mem)
    case ((mask, mem), s"mem[$r] = $x") => (mask, updateMem(mask, r.toLong, x.toLong, mem))
  }

  def parseMask(m: String) : Tuple2[Long, Long] = m.foldLeft((0L, 0L)) {
    case ((a, b), '0') => ((a << 1) + 1, (b << 1))
    case ((a, b), '1') => (a << 1, (b << 1) + 1)
    case ((a, b), 'X') => (a << 1, b << 1)
    case _ => throw new Exception
  }

  def updateMem(mask: Mask, addr: Long, x: Long, mem: Mem): Mem = {
    mem.updated(addr, maskVal(mask, x))
  }

  def maskVal(mask: Mask, x: Long): Long = mask match {
    case (zeroMask, oneMask) => x & (~zeroMask) | oneMask
  }

  def solution(lines: Iterator[String]): BigInt = {
    lines.foldLeft(initState)(exec)._2.foldRight(BigInt(0))((m, a) => a + m._2)
  }

  def solution(filename: String): BigInt = solution(Source.fromFile(filename).getLines)
}

object PartTwo {
  type Mask = Tuple2[Long, Long]
  type Mem = HashMap[Long, Long]
  type State = Tuple2[Mask, Mem]
  def initState: State = ((0, 0), HashMap[Long, Long]())

  def exec(state: State, line: String): State = (state, line) match {
    case ((_, mem),    s"mask = $s") => (parseMask(s), mem)
    case ((mask, mem), s"mem[$r] = $x") => (mask, updateMem(mask, r.toLong, x.toLong, mem))
  }

  def parseMask(m: String) : Tuple2[Long, Long] = m.foldLeft((0L, 0L)) {
    case ((a, b), 'X') => ((a << 1) + 1, (b << 1))
    case ((a, b), '1') => (a << 1, (b << 1) + 1)
    case ((a, b), '0') => (a << 1, b << 1)
    case _ => throw new Exception
  }

  def testBit(x: Long, i: Int): Boolean = (x & (1L << i)) != 0
  def bit(i: Int): Long = 1L << i

  def floatingAddrs(mask: Long): Iterator[Long] = {
    (0 to 35).filter(i => testBit(mask, i)).map(bit)
             .toSet.subsets().map(x => x.sum)
  }
  
  def addresses(addr: Long, mask: Mask): Iterator[Long] = mask match {
    case (floating, set) => floatingAddrs(floating).map(_ | (addr & ~floating | set))
  }

  def updateMem(mask: Mask, addr: Long, x: Long, mem: Mem): Mem = {
    addresses(addr, mask).foldLeft(mem)((m, a) => m.updated(a, x))
  }

  def solution(lines: Iterator[String]): BigInt = {
    lines.foldLeft(initState)(exec)._2.foldRight(BigInt(0))((m, a) => a + m._2)
  }

  def solution(filename: String): BigInt = solution(Source.fromFile(filename).getLines)
}
