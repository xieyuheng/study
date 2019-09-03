package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.example.collection._

object ab extends ExampleRule {

  // equal number of "a" "b"

  val sentences = Seq(
    "ab",
    "abab",
    "aabb",
  )

  val non_sentences = Seq(
    "aab",
  )

  def main = ab

  def ab = Rule(
    "ab", Map(
      "head_a" -> Seq("a", b),
      "head_b" -> Seq("b", a)))

  def a: Rule = Rule(
    "a", Map(
      "one" -> Seq("a"),
      "more" -> Seq("a", ab),
      "after_b" -> Seq("b", a, a)))

  def b: Rule = Rule(
    "b", Map(
      "one" -> Seq("b"),
      "more" -> Seq("b", ab),
      "after_a" -> Seq("a", b, b)))

}
