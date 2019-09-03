package xieyuheng.partech

import xieyuheng.partech.example._

object GeneratorTest extends App {
  def show(ex: ExampleRule): Unit = {
    Generator(ex.main)
      .take(100)
      .foreach { case tree =>
        println(tree.toStr)
      }
  }

  ExampleRule.examples.foreach(show)
}
