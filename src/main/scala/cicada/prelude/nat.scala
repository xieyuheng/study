package cicada.prelude

import cicada._
import cicada.dsl._
import cicada.pretty._
import cicada.json.rw._

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

  ep("nat_t")
  ep("zero_t")
  ep("succ_t")
  ep("succ_t" ap $("prev" -> "zero_t"))
  ep("succ_t" ap $("prev" -> "zero_t") dot "prev")

  eval("succ_t" ap $("prev" -> "zero_t"), module) match {
    case Right(value) =>
      println(nat.toInt(value))
    case Left(errorMsg) =>
      println(errorMsg)
  }
}
