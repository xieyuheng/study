package xieyuheng.partech

import xieyuheng.partech.example._

object GeneratorTest extends App {
  import xieyuheng.partech.ruleDSL._
  import example._

  def show(ex: ExampleRule): Unit = {
    val rule = ex.main

    Generator(rule)
      .take(100)
      .foreach { case tree => println(tree.toStr()) }
  }

  ExampleRule.examples.foreach(show)
}
