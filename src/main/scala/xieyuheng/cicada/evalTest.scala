package xieyuheng.cicada

import xieyuheng.cicada.dsl._

object evalTest extends App {
  val `eval should eval Type to TypeOfType` = {
    val env = Env()
    for {
      t <- eval(Type(), env)
    } assert(t.isInstanceOf[TypeOfType])
  }

  val `eval should eval undefined Var to NeutralValue` = {
    val env = Env()
    for {
      neu <- eval(Var("x"), env)
    } assert(neu == NeutralValue(VarNeutral("x")))
  }

  val `eval should eval defined Var to value` = {
    val xId = Id("x")
    val yId = Id("y")

    val env = Env()
      .defValue("x", TypeOfType(xId))
      .defValue("y", TypeOfType(yId))

    for {
      x <- eval("x", env)
      y <- eval("y", env)
    } {
      assert(x == TypeOfType(xId))
      assert(y == TypeOfType(yId))
    }
  }
}
