import scala.io.Source
import scala.collection.mutable.Set
object Main extends App {
  val file = "input"
  val insts = InstructionOps.instructions(Source.fromFile(file))
  val mach = new Machine(insts)
  println(mach.last)

  def swapInst(inst: Instruction): Instruction = inst match {
    case Acc(n) => Acc(n)
    case Jmp(n) => Nop(n)
    case Nop(n) => Jmp(n)
  }

  def fixMach(insts: Array[Instruction]): Option[Int] = {
    def swapped(i: Int): Array[Instruction] = insts.updated(i, swapInst(insts(i)))
    def trySwap(i: Int): Option[Int] = i match {
      case 0 => None
      case x => new Machine(swapped(x)).haltVal orElse trySwap(i - 1)
    }
    trySwap(insts.size - 1)
  }

  println(fixMach(insts).get)

}

abstract class Instruction
case class Nop(value: Int) extends Instruction
case class Acc(value: Int) extends Instruction
case class Jmp(value: Int) extends Instruction

object InstructionOps {
  def toInst(line: String): Instruction = line match {
    case s"nop $n" => Nop(n.toInt)
    case s"acc $n" => Acc(n.toInt)
    case s"jmp $n" => Jmp(n.toInt)
  }

  def toString(inst: Instruction): String = inst match {
    case Nop(n) => s"nop $n"
    case Acc(n) => s"acc $n"
    case Jmp(n) => s"jmp $n"
  }

  def instructions(source: Source): Array[Instruction] = {
    source.getLines.toArray.map(toInst)
  }
}

class Machine(insts: Array[Instruction]) extends Iterable[Int] {
  case class State(inst: Int, acc: Int)
  def step(inst: Int, acc: Int, instruction: Instruction): State = instruction match {
    case Acc(n) => State(inst + 1, acc + n)
    case Jmp(n) => State(inst + n, acc)
    case Nop(_) => State(inst + 1, acc)
  }

  def step(inst: Int, acc: Int): State = step(inst, acc, insts(inst))

  def step(state: State): State = state match {
    case State(inst, acc) => step(inst, acc)
  }

  def haltVal: Option[Int] = {
    var visited = Set[Int]()
    var state = State(0, 0)
    while (!visited(state.inst) && state.inst < insts.size) {
      visited.addOne(state.inst)
      state = step(state)
    }
    if (state.inst == insts.size) Some(state.acc) else None
  }

  def iterator: Iterator[Int] = new MachineIterator

  class MachineIterator extends Iterator[Int] {
    var state = State(0, 0)
    var visited = Set[Int]()
    def hasNext: Boolean = step(state) match {
      case State(inst, _) => !visited(inst) && inst < insts.size
    }
    def next(): Int = {
      state = step(state)
      visited.addOne(state.inst)
      state.acc
    }
  }
}
