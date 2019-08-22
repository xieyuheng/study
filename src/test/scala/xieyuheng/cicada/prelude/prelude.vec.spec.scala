import org.scalatest._

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.prelude._

class preludeVecSpec extends FlatSpec with Matchers {
  "prelude.vec" should "work" in {
    implicit val module = vec.env

    ep("vec_t")
    ep("null_vec_t")
    ep("cons_vec_t")
  }
}
