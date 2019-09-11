package xieyuheng.mini_tt

import xieyuheng.partech._
import xieyuheng.partech.pretty._
import xieyuheng.partech.example._

object grammar_test extends App {
  def test(): Unit = {
    val rule = grammar.start
    val lexer = grammar.lexer
    val sentences = grammar.sentences
    val non_sentences = grammar.non_sentences

    sentences.foreach { case text =>
      Parser(lexer, rule).parse(text) match {
        case Right(tree) => {}
        case Left(error) =>
          println(s"[ParserTest] should parse")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          println(s"- error: ${error}")
          throw new Exception()
      }
    }

    non_sentences.foreach { case text =>
      Parser(lexer, rule).parse(text) match {
        case Right(tree) =>
          println(s"[ParserTest] should not parse")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          println(s"- tree: ${prettyTree(tree)}")
          throw new Exception()
        case Left(error) => {}
      }
    }
  }

  test()
}
