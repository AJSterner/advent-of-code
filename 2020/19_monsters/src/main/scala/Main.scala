import scala.io.Source
import MonsterMessages.countMatches

object Main extends App {
  val lines = Source.fromFile("input").getLines().toList
  println(countMatches(lines, pt1 = true))
  println(countMatches(lines, pt1 = false))
}

object MonsterMessages {
  sealed abstract class Rule
  case class Lit(c: Char) extends Rule
  case class Chain(rs: List[String]) extends Rule
  case class Or(r0: Rule, r1: Rule) extends Rule

  def parseRule(s: String) = {
    def chain(s: String) = Chain(s.split(' ').toList)
    s match {
      case s"""$k: "$c"""" => (k -> Lit(c.head))
      case s"$k: $r0 | $r1" => (k -> Or(chain(r0), chain(r1)))
      case s"$k: $r" => (k -> chain(r))
    }
  }

  def parseRules(lines: Iterable[String]) = lines.map(parseRule).toMap

  def parseRulesPart2(lines: Iterable[String]) =
    parseRules(lines) + parseRule("8: 42 | 42 8") + parseRule("11: 42 31 | 42 11 31")

  def matchesRules(rules: Map[String, Rule])(msg: String) = {
    type Cont = () => Boolean
    type Succ = (List[Char], Cont) => Boolean
    def m(r: Rule, s: List[Char], succ: Succ, fail: Cont): Boolean = r match {
      case Lit(c) => if (s.nonEmpty && s.head == c) succ(s.tail, fail) else fail()
      case Chain(r :: rs) => m(rules(r), s, (s, f) => m(Chain(rs), s, succ, f), fail)
      case Chain(Nil) => succ(s, fail)
      case Or(r0, r1) => m(r0, s, succ, () => m(r1, s, succ, fail))
    }
    m(rules("0"), msg.toList, (s, f) => s.isEmpty || f(), () => false)
  }

  def splitOnEmpty(lines: List[String]) = lines.span(_.nonEmpty) match {
      case (as, b :: bs) => (as, bs)
      case (_, Nil) => throw new Exception("no empty line")
  }

  def countMatches(lines: List[String], pt1: Boolean = true): Int = {
    val (rules, msgs) = splitOnEmpty(lines)
    val rs = if (pt1) parseRules(rules) else parseRulesPart2(rules)
    msgs.count(matchesRules(rs))
  }
}
