package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object nat {

  val env = Env()

  .defType("nat_t", $(),
    members = $(
      "zero_t" -> $(),
      "succ_t" -> $("prev" -> The("nat_t"))))

  .defFn("nat_add",
    args = $(
      "x" -> The("nat_t"),
      "y" -> The("nat_t")),
    ret = The("nat_t"),
    body = Case("x", $(
      "zero_t" -> "y",
      "succ_t" -> ("succ_t" ap $(
        "prev" -> ("nat_add" ap $(
          "x" -> ("x" dot "prev"),
          "y" -> "y")))))))

  val zero: Exp = "zero_t"
  val one = "succ_t" ap $("prev" -> zero)
  val two = "succ_t" ap $("prev" -> one)
  val three = "succ_t" ap $("prev" -> two)
  val four = "succ_t" ap $("prev" -> three)

  def toInt(value: Value): Int = {
    val json = writeJs(walk.deepSelf(value))
    jsonToInt(json)
  }

  def jsonToInt(json: ujson.Value): Int = {
    json("name").str match {
      case "zero_t" => 0
      case "succ_t" =>
        1 + jsonToInt(json("map")("prev"))
    }
  }
}

object natTest extends App {
  import nat._

  implicit val module = nat.env

  util.evalOnRight("succ_t" ap $("prev" -> "zero_t")) {
    case value =>
      assert(nat.toInt(value) == 1)
  }

  util.evalOnRight("succ_t" ap $("prev" -> "zero_t") dot "prev") {
    case value =>
      assert(nat.toInt(value) == 0)
  }

  util.evalOnRight("nat_add" ap $("x" -> one, "y" -> one)) {
    case value =>
      assert(nat.toInt(value) == 2)
  }

  util.evalOnRight("nat_add" ap $("x" -> two, "y" -> two)) {
    case value =>
      assert(nat.toInt(value) == 4)
  }
}
