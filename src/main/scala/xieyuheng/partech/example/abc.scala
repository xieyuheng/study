package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.dsl._
import xieyuheng.partech.example.collection._

object abc {

  // ambiguous grammar
  // language = "a"^m "b"^n "c"^n | "a"^p "b"^p "c"^q

  def abc = Rule(
    "abc", Map(
      "a_bc" -> Seq(a, bc),
      "ab_c" -> Seq(ab, c)))

  def a: Rule = Rule(
    "a", Map(
      "one" -> Seq("a"),
      "more" -> Seq("a", a)))

  def bc: Rule = Rule(
    "bc", Map(
      "one" -> Seq("bc"),
      "more" -> Seq("b", bc, "c")))

  def ab: Rule = Rule(
    "ab", Map(
      "one" -> Seq("ab"),
      "more" -> Seq("a", ab, "b")))

  def c: Rule = Rule(
    "c", Map(
      "one" -> Seq("c"),
      "more" -> Seq("c", c)))

}
