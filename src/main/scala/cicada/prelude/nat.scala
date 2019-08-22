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
