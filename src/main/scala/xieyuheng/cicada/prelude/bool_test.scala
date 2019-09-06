package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object bool_test extends Module with App {

  import_all(bool)

  eval_on_right("true") {
    case value =>
      assert(bool.to_boolean(value) == true)
  }

  eval_on_right("false") {
    case value =>
      assert(bool.to_boolean(value) == false)
  }

  eval_on_right("true_t") {
    case value =>
      assert(bool.to_boolean(value) == true)
  }

  eval_on_right("false_t") {
    case value =>
      assert(bool.to_boolean(value) == false)
  }
}
