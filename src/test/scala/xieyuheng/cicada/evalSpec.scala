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

  val nat = Env()
    .defineUnion("Nat", MultiMap(), List("Zero", "Succ"))
    .defineRecord("Zero", MultiMap())
    .defineRecord("Succ", MultiMap("prev" -> Var("Nat")))

  it should "eval nat" in {
    for {
      n <- eval(Var("Nat"), nat)
      z <- eval(Var("Zero"), nat)
      s <- eval(Var("Succ"), nat)
      one <- eval(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), nat)
      z1 <- eval(Field(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), "prev"), nat)
    } {
      println(s"n: ${n}")
      println(s"z: ${z}")
      println(s"s: ${s}")
      println(s"one: ${one}")
      println(s"z1: ${z1}")
    }
  }
}
