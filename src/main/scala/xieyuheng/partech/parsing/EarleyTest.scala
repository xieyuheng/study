package xieyuheng.partech

import xieyuheng.partech.pretty._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object EarleyTest extends App {

  def lex(text: String): List[Word] = {
    Lexer.default.lex(text) match {
      case Right(words) => words
      case Left(error) =>
        println(error)
        throw new Exception()
    }
  }

  def parseShow(rule: Rule, text: String): Unit = {
    Earley.init(lex(text), rule).nextTree() match {
      case Some(tree) =>
        println(prettyTree(tree))
      case None => {}
        println(s"rule: ${rule.name}, fail on text: ${text}")
    }
  }

  val `a-a+a` = {

    // S:E -> E
    // E:EQF -> E Q F
    // E:F -> F
    // F:a -> a
    // Q:+ -> +
    // Q:- -> -

    // def S = Rule(
    //   "S", Map(
    //     "E" -> List(E),
    //   ))

    def S = E

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

    assert(Earley.init(lex("a-a+a"), S).recognize)
    assert(Earley.init(lex("a-a+a+a-a+a"), S).recognize)
    assert(!Earley.init(lex("a-a+a+aa+a"), S).recognize)

    parseShow(S, "a-a+a")
    parseShow(S, "a-a+a+a-a+a")
  }

  val `Sb` = {

    // S:SS -> S S
    // S:b -> b

    def S: Rule = Rule(
      "S", Map(
        "SS" -> List(S, S),
        "b" -> List("b"),
      ))

    assert(Earley.init(lex("b"), S).recognize)
    assert(Earley.init(lex("b b"), S).recognize)
    assert(Earley.init(lex("b b b"), S).recognize)
    assert(Earley.init(lex("b b b b"), S).recognize)

    parseShow(S, "b")
    parseShow(S, "b b")
    parseShow(S, "b b b")
  }
}
