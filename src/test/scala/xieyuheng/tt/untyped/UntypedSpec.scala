import org.scalatest._

import xieyuheng.tt.untyped._

class UntypedSpec extends FlatSpec with Matchers {
  "freshen" should "generate new name not used" in {
    val usedNames = Set("x", "x*")
    val freshName = Util.freshen(usedNames, "x")
    assert(!usedNames.contains(freshName))
  }

  it should "add * to the end of name" in {
    val usedNames = Set("x", "x*")
    val freshName = Util.freshen(usedNames, "x")
    assert(freshName == "x**")
  }

  "eval" should "eval Lambda" in {
    val exp = Lambda("x", Lambda("y", Var("y")))
    assert(exp.eval(Env()) ==
      Right(Closure(Env(), "x", Lambda("y", Var("y")))))
  }

  it should "eval Apply" in {
    val exp = Apply(Lambda("x", Var("x")), Lambda("x", Var("x")))
    assert(exp.eval(Env()) ==
      Right(Closure(Env(), "x", Var("x"))))
  }

  "readBack" should "readBack normal form" in {
    for {
      value <- Apply(
        Lambda("x", Lambda("y", Apply(Var("x"), Var("y")))),
        Lambda("x", Var("x")))
      .eval(Env())
      norm <- value.readBack(Set())
    } yield assert(norm == Lambda("y", Var("y")))
  }

  "Module" can "define and run exp" in {
    val m = Module()
    m.define("id", Lambda("x", Var("x")))
    assert(m.run(Var("id")) == Right(Lambda("x", Var("x"))))
    assert(m.run(Lambda("x", Var("x"))) == Right(Lambda("x", Var("x"))))
  }
}
