package xieyuheng.partech.lexerless

import xieyuheng.partech.lexerless.example._

object TreeToTest extends App {
  def test(ex: ExampleRule): Unit = {
    val rule = ex.main

    ex.sentences.foreach { case text =>
      Parser(rule).parsing(text).nextTree match {
        case Some(tree) =>
          ex.treeToMainType match {
            case Some(treeTo) => println(treeTo(tree))
            case None => {}
          }
        case None =>
          println(s"[TreeToTest]")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          throw new Exception()
      }
    }
  }

  ExampleRule.examples.foreach(test)
}
