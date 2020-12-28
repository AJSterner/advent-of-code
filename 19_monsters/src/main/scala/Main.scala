import scala.util.matching.Regex
import scala.io.Source
import MonsterMessages.{parseMessages, matchesRule, toRegex}
object Main extends App {
  val (rules, msgs) = parseMessages(Source.fromFile("input").getLines.toList)
  val r0 = matchesRule(toRegex(rules(0))) _
  println(msgs.count(r0))
}

object MonsterMessages {
  sealed abstract class Rule
  case class Lit(c: Char) extends Rule
  case class Chain(rs: List[Rule]) extends Rule
  case class Or(r0: Rule, r1: Rule) extends Rule

  def parseRules(lines: IterableOnce[String]): DefaultHashMap[Int, Rule] = {
    val rawRules: Map[Int, String] = lines.map({case s"$k: $r" => (k.toInt, r)}).toMap
    new DefaultHashMap[Int, Rule]((k, h) => {
      def chain(s: String) = Chain(s.split(' ').toList.map(r => h(r.toInt)))
      rawRules(k) match {
        case s""""$c"""" => Lit(c.head)
        case s"$r0 | $r1" => Or(chain(r0), chain(r1))
        case rs => chain(rs)
      }
    })
  }

  def parseMessages(lines: List[String]) = {
    lines.span(_.nonEmpty) match {
      case (rs, _ :: ms) => (parseRules(rs), ms)
      case _ => throw new Exception("no blank line")
    }
  }

  def toRegexStr(r: Rule): String = r match {
    case Lit(c) => c.toString()
    case Chain(rs) => s"(?:${rs.foldLeft("")((a, r) => a ++ toRegexStr(r))})"
    case Or(r0, r1) => s"(?:${toRegexStr(r0)}|${toRegexStr(r1)})"
  }

  def toRegex(r: Rule): Regex = raw"^${toRegexStr(r)}\z".r

  def matchesRule(rule: Regex)(msg: String): Boolean = {
    rule.matches(msg)
  }
}

object MonsterMessages2 {
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

  def parseRules(lines: List[String]): Map[String, Rule] = lines.map(parseRule).toMap

  def parseRulesPart2(lines: List[String]): Map[String, Rule] = {
    parseRules(lines) + parseRule("8: 42 | 42 8") + parseRule("11: 42 31 | 42 11 31")
  }

  def matchesRules(rules: Map[String, Rule])(msg: String) = {
    def m(r: Rule, s: List[Char], succ: List[Char] => Boolean, fail: () => Boolean): Boolean = r match {
      case Lit(c) => if (s.nonEmpty && s.head == c) succ(s.tail) else fail()
      case Chain(r :: rs) => m(rules(r), s, s => m(Chain(rs), s, succ, fail), fail)
      case Chain(Nil) => succ(s)
      case Or(r0, r1) => m(r0, s, succ, () => m(r1, s, succ, fail))
    }
    m(rules("0"), msg.toList, s => s.isEmpty, () => false)
  }
}

class DefaultHashMap[K, V](defaultFunc: (K, DefaultHashMap[K, V]) => V) extends scala.collection.mutable.HashMap[K, V] {
  override def default(key: K): V = {
    val value = defaultFunc(key, this)
    put(key, value)
    value
  }
}
