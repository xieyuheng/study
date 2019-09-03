package xieyuheng.partech

import xieyuheng.partech.example._

object TreeToTest extends App {
  import xieyuheng.partech.ruleDSL._
  import example._

  def test(ex: ExampleRule): Unit = {
    val rule = ex.main

    ex.sentences.foreach { case text =>
      Parser(rule).parsing(text).nextTree match {
        case Some(tree) => {}
        case None =>
          println(s"[TreeToTest]")
          println(s"- rule: ${rule.name}")
          println(s"- text: ${text}")
          throw new Exception()
      }
    }
  }

  Seq(
    bool_sexp,
    bin_sum,
  ).foreach(test)
}
