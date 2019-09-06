package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._

object list_test extends App {
  implicit val module = list.env

  val zero: Exp = "zero_t"

  val one = "succ_t" ap %("prev" -> zero)

  val threeZeros =
    "cons_t" ap %(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap %(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap %(
          "A" -> "nat_t",
          "head" -> zero,
          "tail" -> "null_t")))))

  val zeroAndOne =
    "cons_t" ap %(
      "A" -> "nat_t",
      "head" -> zero,
      "tail" -> ("cons_t" ap %(
        "A" -> "nat_t",
        "head" -> one,
        "tail" -> "null_t")))

  util.evalOnRight(
    "list_length" ap %(
      "list" -> (zeroAndOne))) {
    case value =>
      assert(nat.to_int(value) == 2)
  }

  val twoZeros = "cdr" ap %(
    "list" -> threeZeros)

  util.evalOnRight(
    "list_length" ap %(
      "list" -> (twoZeros))) {
    case value =>
      assert(nat.to_int(value) == 2)
  }

  val oneZero = "cdr" ap %(
    "list" -> twoZeros)

  util.evalOnRight(
    "list_length" ap %(
      "list" -> (oneZero))) {
    case value =>
      assert(nat.to_int(value) == 1)
  }

  util.evalOnRight(
    "list_length" ap %(
      "list" -> ("list_append" ap %(
        "ante" -> threeZeros,
        "succ" -> threeZeros)))) {
    case value =>
      assert(nat.to_int(value) == 6)
  }
}
