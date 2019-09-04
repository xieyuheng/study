package xieyuheng.partech.lexerless

import xieyuheng.partech.lexerless.example._

object GeneratorTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.start)
      .take(100)
      .foreach { case tree =>
        println(tree.toStr)
      }
  }

  ExampleRule.examples.foreach(show)
}
