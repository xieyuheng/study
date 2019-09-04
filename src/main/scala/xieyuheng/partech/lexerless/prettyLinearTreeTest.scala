package xieyuheng.partech.lexerless

import xieyuheng.partech.lexerless.example._

object prettyLinearTreeTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.start)
      .take(10)
      .foreach { case tree =>
        println(pretty.prettyLinearTree(tree))
      }
  }

  ExampleRule.examples.foreach(show)
}
