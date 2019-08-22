package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._

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

  util.evalPrint("list_t")
  util.evalPrint("null_t")
  util.evalPrint("cons_t")

  util.evalPrint("nat_t")
  util.evalPrint("zero_t")
  util.evalPrint("succ_t")

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

  util.evalPrint(threeZeros)

  val zeroAndOne =
    "cons_t" ap $(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap $(
        "A" -> "nat_t",
        "head" -> one,
        "tail" -> "null_t")))

  util.evalPrint(zeroAndOne)

  util.evalPrint("cons_t" ap $(
    "A" -> "nat_t",
    "head" -> "zero_t",
    "tail" -> "null_t"))

  util.evalPrint("cons_t" ap $(
    "A" -> "nat_t",
    "head" -> "zero_t",
    "tail" -> ("null_t" ap $("A" -> "nat_t"))))

  val twoZeros = "cdr" ap $(
    "list" -> threeZeros)

  val oneZero = "cdr" ap $(
    "list" -> twoZeros)

  util.evalPrint(twoZeros)
  util.evalPrint(oneZero)

  util.evalPrint("list_append" ap $(
    "ante" -> threeZeros,
    "succ" -> threeZeros))
}
