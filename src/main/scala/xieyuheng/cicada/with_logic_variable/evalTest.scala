package xieyuheng.cicada.with_logic_variable

import xieyuheng.cicada.with_logic_variable.expDSL._

object evalTest extends App {
  val `eval should eval Type to TypeOfType` = {
    val env = Env()
    for {
      t <- eval(Type(), env)
    } assert(t.isInstanceOf[TypeOfType])
  }

  val `eval should eval undefined Var to NeuVal` = {
    val env = Env()
    for {
      neu <- eval(Var("x"), env)
    } assert(neu == NeuVal(VarNeu("x")))
  }

  val `eval should eval defined Var to value` = {
    val xId = Id("x")
    val yId = Id("y")

    val env = Env()
      .extend("x" -> DefineVal("x", TypeOfType(xId)))
      .extend("y" -> DefineVal("y", TypeOfType(yId)))

    for {
      x <- eval("x", env)
      y <- eval("y", env)
    } {
      assert(x == TypeOfType(xId))
      assert(y == TypeOfType(yId))
    }
  }
}
