package xieyuheng.partech

object GeneratorTest extends App {
  import xieyuheng.partech.dsl._
  import Example._

  def show(rule: Rule): Unit = {
    Generator(rule)
      .take(100)
      .foreach { case tree => println(tree.toStr()) }
  }

  show(bool_sexp.bool_sexp)
  show(tom_dick_and_harry.tom_dick_and_harry)
  show(tdh.tdh)
  show(tdh_left.tdh_left)
  show(sum.sum)
  show(ab.ab)
  show(abc.abc)
}
