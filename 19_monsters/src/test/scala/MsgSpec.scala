
import org.scalatest._
import matchers._
import flatspec._

import MonsterMessages._
class MsgSpec extends AnyFlatSpec with should.Matchers {
    val testRulesStr = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b""""
    val testRules = parseRules(testRulesStr.linesIterator)
    val testRule0 = toRegex(testRules(0))
    val matchRule0 = matchesRule(testRule0) _

    "test rule 0" should "match ababbb" in {
        assert(matchRule0("ababbb"))
    }

    "test rule 0" should "not match bababa" in {
        assert(!matchRule0("bababa"))
    }

    "test rule 0" should "match abbbab" in {
        assert(matchRule0("abbbab"))
    }

    "test rule 0" should "not match aaabbb" in {
        assert(!matchRule0("aaabbb"))
    }

    "test rule 0" should "not match aaaabbb" in {
        assert(!matchRule0("aaaabbb"))
    }

}