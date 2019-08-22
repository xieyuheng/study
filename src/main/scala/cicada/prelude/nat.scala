package cicada.prelude

import cicada._
import cicada.dsl._
import cicada.pretty._

object nat {

  val env = Env()

  .defType("nat_t", $(),
    members = $(
      "zero_t" -> $(),
      "succ_t" -> $("prev" -> The("nat_t"))))


  def toInt(value: Value): Int = {
    import cicada.json.rw._
    import upickle.default._

    println(write(value, indent = 2))

    value match {
      case MemberTypeValue(name, map, superName, bind) =>
        name match {
          case "zero_t" => 0
          case "succ_t" =>
            map.get("prev") match {
              case Some(value) =>
                1 + toInt(walk.deep(value, bind))
              case _ =>
                throw new Exception(
                  s"value is not a concrete nat: ${prettyValue(value)}")
            }
          case _ =>
            throw new Exception(
              s"value is not a concrete nat: ${prettyValue(value)}")
        }
      case _ =>
        throw new Exception(
          s"value is not a concrete nat: ${prettyValue(value)}")
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
