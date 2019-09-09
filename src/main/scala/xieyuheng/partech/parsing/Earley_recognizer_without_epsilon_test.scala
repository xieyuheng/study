package xieyuheng.partech

import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object Earley_recognizer_without_epsilon_test extends App {
  def test(rule: Rule, lexer: Lexer, text: String): Unit = {
    lexer.lex(text).foreach { case words =>
      val parsing = Earley_recognizer_without_epsilon.init(text, words, rule)
      parsing.run()
    }
  }

  def S = Rule(
    "S", Map(
      "E" -> List(E),
    ))


  def E: Rule = Rule(
    "E", Map(
      "EQF" -> List(E, Q, F),
      "F" -> List(F),
    ))

  def F = Rule(
    "F", Map(
      "a" -> List("a"),
    ))

  def Q = Rule(
    "Q", Map(
      "+" -> List("+"),
      "-" -> List("-"),
    ))

  test(S, Lexer.default, "a-a+a")

  test(S, Lexer.default, "a-a+a+a-a+a")

  test(S, Lexer.default, "a-a+a+aa+a")

}
