package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.json.rw._

import upickle.default._

object nat extends Module {

  define_type("nat_t", %(),
    members = %(
      "zero_t" -> %(),
      "succ_t" -> %("prev" -> the("nat_t"))))

  define_fn("nat_add",
    args = %(
      "x" -> the("nat_t"),
      "y" -> the("nat_t")),
    ret = the("nat_t"),
    body = choice("x", %(
      "zero_t" -> "y",
      "succ_t" -> ("succ_t" ap %(
        "prev" -> ("nat_add" ap %(
          "x" -> ("x" dot "prev"),
          "y" -> "y")))))))

  val zero: Exp = "zero_t"
  val one = "succ_t" ap %("prev" -> zero)
  val two = "succ_t" ap %("prev" -> one)
  val three = "succ_t" ap %("prev" -> two)
  val four = "succ_t" ap %("prev" -> three)

  def to_int(value: Value): Int = {
    val json = writeJs(walk.deepSelf(value))
    json_to_int(json)
  }

  def json_to_int(json: ujson.Value): Int = {
    json("name").str match {
      case "zero_t" => 0
      case "succ_t" =>
        1 + json_to_int(json("map")("prev"))
    }
  }

}
