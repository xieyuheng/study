package xieyuheng.partech

import xieyuheng.partech.example._

object TreeToTest extends App {
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
    // tom_dick_and_harry,
    // tdh,
    // tdh_left,
    bin_sum,
    dec_sum,
    ab,
    // abc,
  ).foreach(test)

  ExampleRule.examples.foreach(test)
}
