package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object nat {

  val env = Env()

  .defType("nat_t", $(),
    members = $(
      "zero_t" -> $(),
      "succ_t" -> $("prev" -> The("nat_t"))))

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

object natTest extends Test {
  implicit val module = nat.env

  util.evalOnRight("succ_t" ap $("prev" -> "zero_t")) { value =>
    assert(nat.toInt(value) == 1)
  }

  util.evalOnRight("succ_t" ap $("prev" -> "zero_t") dot "prev") { value =>
    assert(nat.toInt(value) == 0)
  }
}
