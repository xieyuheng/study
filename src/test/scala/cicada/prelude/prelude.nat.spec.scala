import org.scalatest._

import cicada._
import cicada.dsl._
import cicada.pretty._
import cicada.prelude._

class preludeNatSpec extends FlatSpec with Matchers {
  "prelude.nat" should "work" in {
    implicit val module = nat.env

    ep("nat_t")
    ep("zero_t")
    ep("succ_t")
    ep("succ_t" ap $("prev" -> "zero_t"))
    ep("succ_t" ap $("prev" -> "zero_t") dot "prev")

    eval("succ_t" ap $("prev" -> "zero_t"), module) match {
      case Right(value) =>
        println(nat.toInt(value))
      case Left(errorMsg) =>
        println(errorMsg)
    }
  }
}
