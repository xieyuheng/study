package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object tom_dick_and_harry extends ExampleRule {

  val sentences = List(
    "tom, dick and harry",
  )

  val non_sentences = List(
    "tom, dick, harry",
  )

  def start = tom_dick_and_harry

  def treeToMainType = None

  def tom_dick_and_harry = Rule(
    "tom_dick_and_harry", Map(
      "name" -> List(name),
      "list" -> List(name_list, " and ", name)))

  def name = Rule(
    "name", Map(
      "tom" -> List("tom"),
      "dick" -> List("dick"),
      "harry" -> List("harry")))

  def name_list = non_empty_list(name)(", ")

}
