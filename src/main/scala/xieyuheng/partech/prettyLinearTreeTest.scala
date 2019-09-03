package xieyuheng.partech

import xieyuheng.partech.example._

object prettyLinearTreeTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.main)
      .take(10)
      .foreach { case tree =>
        println(pretty.prettyLinearTree(tree))
      }
  }

  ExampleRule.examples.foreach(show)
}
