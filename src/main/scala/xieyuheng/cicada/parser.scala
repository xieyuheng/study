package xieyuheng.cicada

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object parser {
  def exp: Rule = Rule(
    "exp", Map(
      "var" -> Seq(identifier),
      "type" -> Seq("type"),
      "dot" -> Seq(exp, space, ".", space, identifier),
    ))

  def identifier_head_char = Rule(
    "identifier_head_char", Map(
      "lower_case_letter" -> Seq(lower_case_letter),
      "upper_case_letter" -> Seq(upper_case_letter),
      "underscore" -> Seq("_")))

  def identifier_rest_char = Rule(
    "identifier_rest_char", Map(
      "lower_case_letter" -> Seq(lower_case_letter),
      "upper_case_letter" -> Seq(upper_case_letter),
      "digit_char" -> Seq(digit_char),
      "underscore" -> Seq("_")))

  def identifier = Rule(
    "identifier", Map(
      "identifier" -> Seq(identifier_head_char, list(identifier_rest_char)(""))))
}
