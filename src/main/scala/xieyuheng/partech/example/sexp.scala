package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object sexp extends ExampleRule {

  def lexer = Lexer.default

  def sentences = List(
    "()",
    "(a b c)",
    "n",
    "(a b (c))",
    "(((a)) b (c))",
  )

  def non_sentences = List(
    "(",
    "())",
  )

  def identifier = WordPred (
    "identifier", { case word =>
      word.headOption match {
        case Some(char) =>
          val head_set = lower_case_char_set ++ upper_case_char_set + '_'
          val tail_set = head_set ++ digit_char_set
          head_set.contains(char) && wordInCharSet(tail_set)(word.tail)
        case None => false
      }
    })

  def start = sexp

  def sexp: Rule = Rule(
    "sexp", Map(
      "null" -> List("(", ")"),
      "atom" -> List(identifier),
      "sexp_list" -> List("(", non_empty_list(sexp), ")"),
    ))
}
