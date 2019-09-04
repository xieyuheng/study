package xieyuheng.partech.lexerless

import xieyuheng.partech.lexerless.example._

object prettyTreeTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.start)
      .take(10)
      .foreach { case tree =>
        println(pretty.prettyTree(Tree.fromLinearTree(tree)))
      }
  }

  ExampleRule.examples.foreach(show)
}
