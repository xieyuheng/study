package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._
import xieyuheng.cicada.with_logic_variable.pretty._

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

    def list_map_succ(exp: Exp) =
      "list_map" ap %(
        "A" -> "nat_t",
        "B" -> "nat_t",
        "f" -> fn(%("x" -> the("nat_t")), the("nat_t"), "succ_t" ap %("prev" -> "x")),
        "list" -> exp)

    eval_on_right(list_map_succ("three_zeros")) {
      case value =>
        println(pretty_val(walk.deepSelf(value)))
    }

    eval_on_right(list_map_succ(list_map_succ("three_zeros"))) {
      case value =>
        println(pretty_val(walk.deepSelf(value)))
    }

    eval_on_right(list_map_succ(list_map_succ(list_map_succ("three_zeros")))) {
      case value =>
        println(pretty_val(walk.deepSelf(value)))
    }
  }

}
