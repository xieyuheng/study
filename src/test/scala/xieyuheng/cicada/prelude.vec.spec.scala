import org.scalatest._

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._

class preludeVecSpec extends FlatSpec with Matchers {
  "prelude.vec" should "work" in {
    implicit val module = prelude.vec

    ep("vec_t")
    ep("null_vec_t")
    ep("cons_vec_t")
  }
}
