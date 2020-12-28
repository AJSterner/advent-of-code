import scala.io.Source
import NewMath._
object Main extends App {

  val test = "1 * 2 + 3 + 4 + 5"
  println(expToStr(parseExp(test), true))
  println(expToStr(addPrec(parseExp(test)), true))

  val input = Source.fromFile("input").getLines.toList
  println(input.map(eval).sum)
  println(input.map(evalAdv).sum)
  println(input.foldLeft(BigInt(0))((a, s) => a + evalAdv(s)))
}

object NewMath {
  sealed abstract class Exp
  case class Val(n: Long) extends Exp
  case class Op(e0: Exp, op: Char, e1: Exp) extends Exp
  case class Paren(e: Exp) extends Exp

  def parseExp(s: String): Exp = parseExp(s.filter(_ != ' ').toList)
  def parseExp(s: List[Char]): Exp = {
    def withExp(e: Exp, s: List[Char], cont: (Exp, List[Char]) => Exp): Exp = s match {
      case op :: ds if (op == '+' || op == '*') =>
        withOp(e, op, ds, (e, s) => withExp(e, s, cont))
      case ')' :: ds => cont(e, ds)
      case Nil => cont(e, Nil)
      case _ => throw new Exception("withExp")
    }

    def withOp(e0: Exp, op: Char, s: List[Char], cont: (Exp, List[Char]) => Exp): Exp = {
      nextExp(s, (e1, s) => cont(Op(e0, op, e1), s))
    }

    def nextExp(s: List[Char], cont: (Exp, List[Char]) => Exp): Exp = s match {
      case d :: ds if d.isDigit => {
        val v = (d :: ds).takeWhile(_.isDigit).mkString.toLong
        cont(Val(v), ds.dropWhile(_.isDigit))
      }
      case '(' :: ds => p(ds, (e, s) => cont(Paren(e), s))
      case _ => throw new Exception("exp")
    }

    def p(s: List[Char], cont: (Exp, List[Char]) => Exp): Exp = {
      nextExp(s, (e, s) => withExp(e, s, cont))
    }
    p(s, (e, _) => e)
  }

  def expToStr(e: Exp, evalParens: Boolean = false): String = {
    def ts(e: Exp): String = e match {
      case Val(n) => n.toString()
      case Paren(e) => "(" ++ ts(e) ++ ")"
      case Op(e0, op, e1) => {
        val s = ts(e0) ++ s" $op " ++ ts(e1)
        if (evalParens) s"($s)" else s
      }
    }
    ts(e)
  }

  def eval(e: Exp): Long = e match {
    case Op(e0, '+', e1) => eval(e0) + eval(e1)
    case Op(e0, '*', e1) => eval(e0) * eval(e1)
    case Paren(e) => eval(e)
    case Val(n) => n
    case Op(_, op, _) => throw new Exception(s"Operator ($op) not supported")
  }

  def eval(s: String): Long = eval(parseExp(s))

  def evalAdv(s: String): Long = eval(addPrec(parseExp(s)))

  def addPrec(e: Exp): Exp = e match {
    case Val(n) => Val(n)
    case Paren(e) => Paren(addPrec(e))
    // Right side can only be Val or Parens until rotated, than only add
    case Op(Op(e0, op, e1), '+', e2) => addPrec(Op(addPrec(e0), op, Op(addPrec(e1), '+', addPrec(e2))))
    case Op(e0, op, e1) => Op(addPrec(e0), op, addPrec(e1))
  }
}