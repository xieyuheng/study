import org.scalatest._
import xieyuheng.tt.tartlet._

class TartletSpec extends FlatSpec with Matchers {
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
      Right(ValueLambda(EnvClosure(Env(), "x", Lambda("y", Var("y"))))))
  }

  it should "eval Apply" in {
    val exp = Apply(Lambda("x", Var("x")), Lambda("x", Var("x")))
    assert(exp.eval(Env()) ==
      Right(ValueLambda(EnvClosure(Env(), "x", Var("x")))))
  }

  "Module" can "define" in {
    var m = Module()

    m.claim("three", Nat)
    m.define("three", Add1(Add1(Add1(Zero))))

    m.claim("+", Arrow(Nat, Arrow(Nat, Nat)))
    m.define("+",
      Lambda("n", Lambda("k",
        IndNat(
          Var("n"),
          Lambda("_", Nat),
          Var("k"),
          Lambda("prev", Lambda("almost",
            Add1(Var("almost"))))))))

    m.run(Var("three"))
    // The(Nat,Add1(Add1(Add1(Zero))))

    m.run(Apply(Var("+"), Var("three")))
    // The(Pi(_*,Nat,Nat),Lambda(_*,Add1(Add1(Add1(Var(_*))))))

    m.run(Apply(Apply(Var("+"), Var("three")), Var("three")))
    // The(Nat,Add1(Add1(Add1(Add1(Add1(Add1(Zero)))))))
  }
}
