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

  define("zero", "zero_t")
  define("one", "succ_t" ap %("prev" -> "zero"))
  define("two", "succ_t" ap %("prev" -> "one"))
  define("three", "succ_t" ap %("prev" -> "two"))
  define("four", "succ_t" ap %("prev" -> "three"))

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

  define_fn("nat_mul",
    args = %(
      "x" -> the("nat_t"),
      "y" -> the("nat_t")),
    ret = the("nat_t"),
    body = choice("x", %(
      "zero_t" -> "zero_t",
      "succ_t" -> ("nat_add" ap %(
        "x" -> "y",
        "y" -> ("nat_mul" ap %(
          "x" -> ("x" dot "prev"),
          "y" -> "y")))))))

  define_fn("nat_factorial",
    args = %(
      "x" -> the("nat_t")),
    ret = the("nat_t"),
    body = choice("x", %(
      "zero_t" -> "one",
      "succ_t" -> ("nat_mul" ap %(
        "x" -> "x",
        "y" -> ("nat_factorial" ap %(
          "x" -> ("x" dot "prev"))))))))

  define_fn("nat_even_p",
    args = %(
      "x" -> the("nat_t")),
    ret = the("bool_t"),
    body = choice("x", %(
      "zero_t" -> "true_t",
      "succ_t" -> choice("x" dot "prev", %(
        "zero_t" -> "false_t",
        "succ_t" -> ("nat_even_p" ap %("x" -> ("x" dot "prev" dot "prev"))))))))


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
