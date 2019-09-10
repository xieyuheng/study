package xieyuheng.partech

import xieyuheng.partech.example._

object ParserTest extends App {
  def test(ex: ExampleRule): Unit = {
    val rule = ex.start
    val lexer = ex.lexer

    ex.sentences.foreach { case text =>
      Parser(rule, lexer).parse(text) match {
        case Right(tree) =>
          println(pretty.prettyTree(tree))
        case Left(error) =>
          println(s"[ParserTest] should parse")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          println(s"- error: ${error}")
          throw new Exception()
      }
    }
  }

  test(exp)
}
