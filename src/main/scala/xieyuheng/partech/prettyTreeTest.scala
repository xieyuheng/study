package xieyuheng.partech

import xieyuheng.partech.example._

object prettyTreeTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.main)
      .take(10)
      .foreach { case tree =>
        println(pretty.prettyTree(Tree.fromLinearTree(tree)))
      }
  }

  ExampleRule.examples.foreach(show)
}
