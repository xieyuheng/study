package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object nat_test extends Module with App {

  import_all(nat)

  eval_on_right("succ_t" ap %("prev" -> "zero_t")) {
    case value =>
      assert(nat.to_int(value) == 1)
  }

  eval_on_right("succ_t" ap %("prev" -> "zero_t") dot "prev") {
    case value =>
      assert(nat.to_int(value) == 0)
  }

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
