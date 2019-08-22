import org.scalatest._

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.prelude._

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
