package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object tom_dick_and_harry extends ExampleRule {

  val sentences = Seq(
    "tom, dick and harry",
  )

  val non_sentences = Seq(
    "tom, dick, harry",
  )

  def main = tom_dick_and_harry

  def treeToMainType = None

  def tom_dick_and_harry = Rule(
    "tom_dick_and_harry", Map(
      "name" -> Seq(name),
      "list" -> Seq(name_list, " and ", name)))

  def name = Rule(
    "name", Map(
      "tom" -> Seq("tom"),
      "dick" -> Seq("dick"),
      "harry" -> Seq("harry")))

  def name_list = non_empty_list(name)(", ")

}
