import org.scalatest._
import xieyuheng.cicada._

class evalSpec extends FlatSpec with Matchers {
  "eval" should "eval Type to LogicVar" in {
    val env = Env()
    for {
      t <- eval(Type(), env)
    } println(t)
  }

  it should "eval undefined Var to NeutralValue" in {
    val env = Env()
    for {
      neu <- eval(Var("x"), env)
    } assert(neu == NeutralValue(VarNeutral("x")))
  }

  it should "eval defined Var to value" in {
    val env = Env()
      .defineValue("x", LogicVar("#x"))
      .defineValue("y", LogicVar("#y"))

    for {
      x <- eval(Var("x"), env)
      y <- eval(Var("y"), env)
    } {
      assert(x == LogicVar("#x"))
      assert(y == LogicVar("#y"))
    }
  }
}
