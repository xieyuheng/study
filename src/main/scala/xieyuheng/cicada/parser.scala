package xieyuheng.cicada

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object parser {
  def exp = Rule(
    "exp", Map(
      "var" -> Seq(identifier),
      "type" -> Seq("type"),
      "dot" -> Seq(space),
    ))

  val rand = scala.util.Random

  def space = ???

  def identifier = ???
}
