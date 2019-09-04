package xieyuheng.partech.lexerless

import xieyuheng.partech.lexerless.example._

object ParserTest extends App {
  def test(ex: ExampleRule): Unit = {
    val rule = ex.main

    ex.sentences.foreach { case text =>
      Parser(rule).parsing(text).nextTree match {
        case Some(tree) => {}
        case None =>
          println(s"[ParserTest] should parse")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          throw new Exception()
      }
    }

    ex.non_sentences.foreach { case text =>
      Parser(rule).parsing(text).nextTree match {
        case Some(tree) =>
          println(s"[ParserTest] should not parse")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          println(s"- tree: ${pretty.prettyTree(tree)}")
          throw new Exception()
        case None => {}
      }
    }
  }

  ExampleRule.examples.foreach(test)
}
