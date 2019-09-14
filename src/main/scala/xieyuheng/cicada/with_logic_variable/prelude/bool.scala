package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._
import xieyuheng.cicada.with_logic_variable.json.rw._

import upickle.default._

object bool extends Module {

  define_type("bool_t", %(),
    members = %(
      "true_t" -> %(),
      "false_t" -> %()))

  define("true", "true_t")
  define("false", "false_t")

  define_fn("not",
    args = %(
      "x" -> the("bool_t")),
    ret = the("bool_t"),
    body = choice("x", %(
      "true_t" -> "false_t",
      "false_t" -> "true_t")))

  define_fn("and",
    args = %(
      "x" -> the("bool_t"),
      "y" -> the("bool_t")),
    ret = the("bool_t"),
    body = choice("x", %(
      "true_t" -> choice("y", %(
        "true_t" -> "true_t",
        "false_t" -> "false_t")),
      "false_t" -> "false_t")))

  define_fn("or",
    args = %(
      "x" -> the("bool_t"),
      "y" -> the("bool_t")),
    ret = the("bool_t"),
    body = choice("x", %(
      "true_t" -> "true_t",
      "false_t" -> choice("y", %(
        "true_t" -> "true_t",
        "false_t" -> "false_t")))))


  def to_boolean(value: Val): Boolean = {
    val json = writeJs(walk.deepSelf(value))
    json_to_boolean(json)
  }

  def json_to_boolean(json: ujson.Value): Boolean = {
    json("name").str match {
      case "true_t" => true
      case "false_t" => false
    }
  }

}
