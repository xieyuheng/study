package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._

object list_test extends Module with App {

  import_all(list)
  import_all(nat)

  define("three_zeros",
    "cons_t" ap %(
      "A" -> "nat_t",
      "head" -> "zero",
      "tail" -> ("cons_t" ap %(
        "A" -> "nat_t",
        "head" -> "zero",
        "tail" -> ("cons_t" ap %(
          "A" -> "nat_t",
          "head" -> "zero",
          "tail" -> "null_t"))))))

  define("zero_and_one",
    "cons_t" ap %(
      "A" -> "nat_t",
      "head" -> "zero",
      "tail" -> ("cons_t" ap %(
        "A" -> "nat_t",
        "head" -> "one",
        "tail" -> "null_t"))))

  eval_on_right("list_length" ap %("list" -> "zero_and_one")) {
    case value =>
      assert(nat.to_int(value) == 2)
  }

  define("two_zeros", "cdr" ap %("list" -> "three_zeros"))

  eval_on_right("list_length" ap %("list" -> "two_zeros")) {
    case value =>
      assert(nat.to_int(value) == 2)
  }

  define("one_zero", "cdr" ap %("list" -> "two_zeros"))

  eval_on_right("list_length" ap %("list" -> "one_zero")) {
    case value =>
      assert(nat.to_int(value) == 1)
  }

  eval_on_right(
    "list_length" ap %(
      "list" -> ("list_append" ap %(
        "ante" -> "three_zeros",
        "succ" -> "three_zeros")))) {
    case value =>
      assert(nat.to_int(value) == 6)
  }
}
