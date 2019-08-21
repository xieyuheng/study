import org.scalatest._

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._

class preludeNatSpec extends FlatSpec with Matchers {
  "prelude.nat" should "work" in {
    implicit val module = prelude.nat

    ep("nat_t")
    ep("zero_t")
    ep("succ_t")
    ep("succ_t" ap $("prev" -> "zero_t"))
    ep("succ_t" ap $("prev" -> "zero_t") dot "prev")
  }
}
