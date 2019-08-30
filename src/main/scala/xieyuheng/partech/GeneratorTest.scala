package xieyuheng.partech

object GeneratorTest extends App {
  import xieyuheng.partech.dsl._
  import RuleExample._

  def show(rule: Rule): Unit = {
    val gen = Generator.fromRule(rule)

    gen.take(60).foreach { case tree => println(tree.toPretty()) }
    gen.take(60).foreach { case tree => println(tree.toStr()) }
  }

  show(bool_sexp)
  show(tom_dick_and_harry)
  show(tdh)
  show(tdh_left)
  show(sum)
}
