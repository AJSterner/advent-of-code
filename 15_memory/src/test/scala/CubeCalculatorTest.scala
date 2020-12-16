import org.scalatest._
import matchers._
import flatspec._
import Main.nth
class MemorySpec extends AnyFlatSpec with should.Matchers {
  "nth" should "answer the examples correctly" in {
    nth("0,3,6", 4) should be (0)
    nth("0,3,6", 10) should be (0)
    nth("0,3,6", 2020) should be (436)
    nth("1,3,2", 2020) should be (1)
    nth("2,1,3", 2020) should be (10)
    nth("1,2,3", 2020) should be (27)
    nth("2,3,1", 2020) should be (78)
    nth("3,2,1", 2020) should be (438)
    nth("3,1,2", 2020) should be (1836)
  }

  "nth" should "answer the long examples correctly" in {
    nth("0,3,6", 30000000) should be (175594)
    nth("1,3,2", 30000000) should be (2578)
    nth("2,1,3", 30000000) should be (3544142)
    nth("1,2,3", 30000000) should be (261214)
    nth("2,3,1", 30000000) should be (6895259)
    nth("3,2,1", 30000000) should be (18)
    nth("3,1,2", 30000000) should be (362)
  }
}
