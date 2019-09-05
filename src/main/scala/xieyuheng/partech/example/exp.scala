package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object exp {

  def lexer = Lexer.default

  def identifier: String => Boolean = { case word =>
    word.headOption match {
      case Some(char) =>
        val head_set = lower_case_char_set ++ upper_case_char_set + '_'
        val tail_set = head_set ++ digit_char_set
        head_set.contains(char) && wordInCharSet(tail_set)(word.tail)
      case None => false
    }
  }

  def start = exp

  def exp: Rule = Rule(
    "exp", Map(
      "type" -> Seq("type"),
      "var" -> Seq(identifier),
      "case" -> Seq(exp, "case", "{", non_empty_list(case_clause), "}"),
      "dot" -> Seq(exp, ".", identifier),
      "pi" -> Seq("pi", "(", non_empty_list(arg), ")", ":", exp),
      "fn" -> Seq("fn", "(", non_empty_list(arg), ")", ":", exp, "=", exp),
      "ap" -> Seq(exp, "(", non_empty_list(arg), ")"),
    ))

  def case_clause = Rule.seq(
    "case_clause", Seq(
      identifier, "=", ">", exp))

  def arg = Rule(
    "arg", Map(
      "value" -> Seq(identifier, "=", exp),
      "type" ->  Seq(identifier, ":", exp),
      "value_comma" -> Seq(identifier, "=", exp, ","),
      "type_comma" ->  Seq(identifier, ":", exp, ","),
    ))

}
