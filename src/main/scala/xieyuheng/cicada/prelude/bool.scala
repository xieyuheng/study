package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.json.rw._

import upickle.default._

object bool extends Module {

  define_type("bool_t", %(),
    members = %(
      "true_t" -> %(),
      "false_t" -> %()))

  define("true", "true_t")
  define("false", "false_t")

  def to_boolean(value: Value): Boolean = {
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
