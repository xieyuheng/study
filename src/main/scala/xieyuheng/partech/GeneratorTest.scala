package xieyuheng.partech

object GeneratorTest extends App {
  import xieyuheng.partech.dsl._
  import RuleExample._

  val gen = Generator.fromRule(sexp)

  gen.take(10).foreach { case tree => println(tree.toXML()) }
}
