package cicada.prelude

import cicada._
import cicada.dsl._
import cicada.pretty._

object list {

  val env = Env()

  .defType("list_t", $("A" -> Type()),
    members = $(
      "null_t" -> $(),
      "cons_t" -> $(
        "head" -> The("A"),
        "tail" -> The("list_t" ap $("A" -> "A")))))

  .defExp("cdr", Fn(
    args = $(
      "list" -> The("list_t")),
    ret = The("list_t"),
    body = "list" dot "tail"))

  .defFn("list_append",
    args = $(
      "ante" -> The("list_t"),
      "succ" -> The("list_t")),
    ret = The("list_t"),
    body = Case("ante", $(
      "null_t" -> "succ",
      "cons_t" -> ("cons_t" ap $(
        "A" -> ("ante" dot "A"),
        "head" -> ("ante" dot "head"),
        "tail" -> ("list_append" ap $(
          "ante" -> ("ante" dot "tail"),
          "succ" -> "succ")))))))

}

object listTest extends Test {
  implicit val module = list.env
    .importAll(nat.env)

  ep("list_t")
  ep("null_t")
  ep("cons_t")

  ep("nat_t")
  ep("zero_t")
  ep("succ_t")

  val zero: Exp = "zero_t"

  val one = "succ_t" ap $("prev" -> zero)

  val threeZeros =
    "cons_t" ap $(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap $(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> zero,
          "tail" -> "null_t")))))

  ep(threeZeros)

  val zeroAndOne =
    "cons_t" ap $(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap $(
        "A" -> "nat_t",
        "head" -> one,
        "tail" -> "null_t")))

  ep(zeroAndOne)

  ep("cons_t" ap $(
    "A" -> "nat_t",
    "head" -> "zero_t",
    "tail" -> "null_t"))

  ep("cons_t" ap $(
    "A" -> "nat_t",
    "head" -> "zero_t",
    "tail" -> ("null_t" ap $("A" -> "nat_t"))))

  val twoZeros = "cdr" ap $(
    "list" -> threeZeros)

  val oneZero = "cdr" ap $(
    "list" -> twoZeros)

  ep(twoZeros)
  ep(oneZero)

  ep("list_append" ap $(
    "ante" -> threeZeros,
    "succ" -> threeZeros))
}
