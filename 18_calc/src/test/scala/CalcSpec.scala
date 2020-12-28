import org.scalatest._
import matchers._
import flatspec._

import NewMath.{eval, evalAdv}

class CalcSpec extends AnyFlatSpec with should.Matchers {
    "eval" should "evaluate expression correctly with flat precidence" in {
        eval("1 + 2 * 3 + 4 * 5 + 6") should be (71)
        eval("1 + (2 * 3) + (4 * (5 + 6))") should be (51)
        eval("2 * 3 + (4 * 5)") should be (26)
        eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") should be (12240)
        eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") should be (13632)
    }

    "evalAdv" should "evaluate expression correctly" in {
        evalAdv("1 + 2 * 3 + 4 * 5 + 6") should be (231)
        evalAdv("1 + (2 * 3) + (4 * (5 + 6))") should be (51)
        evalAdv("2 * 3 + (4 * 5)") should be (46)
        evalAdv("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") should be (669060)
        evalAdv("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") should be (23340)
    }

    
}