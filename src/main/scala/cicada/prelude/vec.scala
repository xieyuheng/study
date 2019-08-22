package cicada.prelude

import cicada._
import cicada.dsl._
import cicada.pretty._

object vec {

  val env = Env()

  .importAll(nat.env)

  .defType("vec_t", $(
    "A" -> Type(),
    "length" -> The("nat_t")),
    members = $(
      "null_vec_t" -> $(
        "length" -> "zero_t"),
      "cons_vec_t" -> $(
        "n" -> The("nat_t"),
        "length" -> ("succ_t" ap $("prev" -> "n")),
        "head" -> The("A"),
        "tail" -> The("vec_t" ap $("A" -> "A", "length" -> "n")))))

}

object vecTest extends Test {
  implicit val module = vec.env

  ep("vec_t")
  ep("null_vec_t")
  ep("cons_vec_t")
}
