import org.scalatest._

import cicada._
import cicada.dsl._
import cicada.pretty._
import cicada.prelude._

class preludeVecSpec extends FlatSpec with Matchers {
  "prelude.vec" should "work" in {
    implicit val module = vec.env

    ep("vec_t")
    ep("null_vec_t")
    ep("cons_vec_t")
  }
}
