package xieyuheng.partech

import xieyuheng.partech.example._

object TreeToTest extends App {
  def test(ex: ExampleRule): Unit = {
    val rule = ex.start
    val lexer = ex.lexer

    ex.sentences.foreach { case text =>
      Parser(lexer, rule).parse(text) match {
        case Right(tree) =>
          ex.treeToMainType match {
            case Some(treeTo) => println(treeTo(tree))
            case None => {}
          }
        case Left(error) =>
          println(s"[TreeToTest]")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          println(s"- error: ${error}")
          throw new Exception()
      }
    }
  }

  test(ab)
  test(sexp)
}
