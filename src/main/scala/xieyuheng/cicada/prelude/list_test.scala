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

  define("two_zeros", "cdr" ap %("list" -> "three_zeros"))
  define("one_zero", "cdr" ap %("list" -> "two_zeros"))

  val test_list_length = {
    eval_on_right("list_length" ap %("list" -> "zero_and_one")) {
      case value =>
        assert(nat.to_int(value) == 2)
    }

    eval_on_right("list_length" ap %("list" -> "two_zeros")) {
      case value =>
        assert(nat.to_int(value) == 2)
    }

    eval_on_right("list_length" ap %("list" -> "one_zero")) {
      case value =>
        assert(nat.to_int(value) == 1)
    }
  }

  val test_list_append = {
    eval_on_right("list_length" ap %(
      "list" -> ("list_append" ap %(
        "ante" -> "three_zeros",
        "succ" -> "three_zeros")))) {
      case value =>
        assert(nat.to_int(value) == 6)
    }
  }

  val test_list_map = {
    eval_on_right("list_map" ap %(
      "A" -> "nat_t",
      "B" -> "nat_t",
      "f" -> fn(%("x" -> the("nat_t")), the("nat_t"), "succ_t" ap %("prev" -> "x")),
      "list" -> "three_zeros")) {
      case value =>
        println(prettyValue(value))
    }
  }

}
