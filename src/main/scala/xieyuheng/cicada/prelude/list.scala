package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._

object list {

  val env = Env()

  .importAll(nat.env)

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

  .defFn("list_length",
    args = $("list" -> The("list_t")),
    ret = The("nat_t"),
    body = Case("list", $(
      "null_t" -> "zero_t",
      "cons_t" -> ("succ_t" ap $(
        "prev" -> ("list_length" ap $(
          "list" -> ("list" dot "tail"))))))))

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

  // util.evalPrint(threeZeros)

  val zeroAndOne =
    "cons_t" ap $(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap $(
        "A" -> "nat_t",
        "head" -> one,
        "tail" -> "null_t")))

  // util.evalPrint(zeroAndOne)

  // util.evalPrint("cons_t" ap $(
  //   "A" -> "nat_t",
  //   "head" -> "zero_t",
  //   "tail" -> "null_t"))

  // util.evalPrint("cons_t" ap $(
  //   "A" -> "nat_t",
  //   "head" -> "zero_t",
  //   "tail" -> ("null_t" ap $("A" -> "nat_t"))))

  val twoZeros = "cdr" ap $(
    "list" -> threeZeros)

  val oneZero = "cdr" ap $(
    "list" -> twoZeros)

  // util.evalPrint(twoZeros)
  // util.evalPrint(oneZero)

  util.evalOnRight(
    "list_length" ap $(
      "list" -> ("list_append" ap $(
        "ante" -> threeZeros,
        "succ" -> threeZeros)))) { value =>
    assert(nat.toInt(value) == 6)
  }
}
