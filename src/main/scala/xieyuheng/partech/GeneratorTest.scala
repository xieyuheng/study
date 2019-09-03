package xieyuheng.partech

import xieyuheng.partech.example._

object GeneratorTest extends App {
  import xieyuheng.partech.dsl._
  import example._

  def show(ex: ExampleRule): Unit = {
    val rule = ex.main

    Generator(rule)
      .take(100)
      .foreach { case tree => println(tree.toStr()) }
  }

  Seq(
    bool_sexp,
    tom_dick_and_harry,
    tdh,
    tdh_left,
    bin_sum,
    // dec_sum,
    ab,
    abc,
  ).foreach(show)

  // ExampleRule.examples.foreach(show)
}
