package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._
import xieyuheng.cicada.with_logic_variable.pretty._
import xieyuheng.cicada.with_logic_variable.json.rw._

import upickle.default._

object nat_test extends Module with App {

  import_all(nat)
  import_all(bool)

  val test_succ = {
    eval_on_right("succ_t" ap %("prev" -> "zero_t")) {
      case value =>
        assert(nat.to_int(value) == 1)
    }

    eval_on_right("succ_t" ap %("prev" -> "zero_t") dot "prev") {
      case value =>
        assert(nat.to_int(value) == 0)
    }
  }

  val test_nat_add = {
    eval_on_right("nat_add" ap %("x" -> "one", "y" -> "one")) {
      case value =>
        assert(nat.to_int(value) == 2)
    }

    eval_on_right("nat_add" ap %("x" -> "two", "y" -> "two")) {
      case value =>
        assert(nat.to_int(value) == 4)
    }

    eval_on_right("nat_add" ap %("x" -> "two", "y" -> "three")) {
      case value =>
        assert(nat.to_int(value) == 5)
    }
  }

  val test_nat_mul = {
    eval_on_right("nat_mul" ap %("x" -> "one", "y" -> "one")) {
      case value =>
        assert(nat.to_int(value) == 1)
    }

    eval_on_right("nat_mul" ap %("x" -> "two", "y" -> "two")) {
      case value =>
        assert(nat.to_int(value) == 4)
    }

    eval_on_right("nat_mul" ap %("x" -> "two", "y" -> "three")) {
      case value =>
        assert(nat.to_int(value) == 6)
    }
  }

  val test_nat_factorial = {
    eval_on_right("nat_factorial" ap %("x" -> "zero")) {
      case value =>
        assert(nat.to_int(value) == 1)
    }

    eval_on_right("nat_factorial" ap %("x" -> "one")) {
      case value =>
        assert(nat.to_int(value) == 1)
    }

    eval_on_right("nat_factorial" ap %("x" -> "two")) {
      case value =>
        assert(nat.to_int(value) == 2)
    }

    eval_on_right("nat_factorial" ap %("x" -> "three")) {
      case value =>
        assert(nat.to_int(value) == 6)
    }

    eval_on_right("nat_factorial" ap %("x" -> "four")) {
      case value =>
        value
        // PROBLEM the use of deepSelf in nat.to_int cost so much
        // assert(nat.to_int(value) == 24)
    }
  }

  val test_nat_even_p = {
    eval_on_right("nat_even_p" ap %("x" -> "zero")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("nat_even_p" ap %("x" -> "one")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("nat_even_p" ap %("x" -> "two")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("nat_even_p" ap %("x" -> "three")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("nat_even_p" ap %("x" -> "four")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }
  }
}
