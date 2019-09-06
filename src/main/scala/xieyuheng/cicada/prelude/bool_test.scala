package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object bool_test extends Module with App {

  import_all(bool)

  val test_true = {
    eval_on_right("true") {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("true_t") {
      case value =>
        assert(bool.to_boolean(value) == true)
    }
  }

  val test_false = {
    eval_on_right("false") {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("false_t") {
      case value =>
        assert(bool.to_boolean(value) == false)
    }
  }

  val test_not = {
    eval_on_right("not" ap %("x" -> "true")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("not" ap %("x" -> "false")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }
  }

  val test_and = {
    eval_on_right("and" ap %("x" -> "true", "y" -> "true")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("and" ap %("x" -> "true", "y" -> "false")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("and" ap %("x" -> "false", "y" -> "true")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }

    eval_on_right("and" ap %("x" -> "false", "y" -> "false")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }
  }

  val test_or = {
    eval_on_right("or" ap %("x" -> "true", "y" -> "true")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("or" ap %("x" -> "true", "y" -> "false")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("or" ap %("x" -> "false", "y" -> "true")) {
      case value =>
        assert(bool.to_boolean(value) == true)
    }

    eval_on_right("or" ap %("x" -> "false", "y" -> "false")) {
      case value =>
        assert(bool.to_boolean(value) == false)
    }
  }
}
