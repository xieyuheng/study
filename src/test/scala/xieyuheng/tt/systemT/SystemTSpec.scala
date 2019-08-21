import org.scalatest._
import xieyuheng.tt.systemT._

class SystemTSpec extends FlatSpec with Matchers {
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

  "infer" should "infer type of Var" in {
    val ctx = Ctx().ext("x", Nat)

    assert(Var("x").infer(ctx) == Right(Nat))
  }

  "check" should "" in {
    assert(Zero.check(Ctx(), Nat) ==
      Right(()))

    assert(Add1(Zero).check(Ctx(), Nat) ==
      Right(()))

    assert(Lambda("x", Var("x")).check(Ctx(), Arrow(Nat, Nat)) ==
      Right(()))

    assert(Lambda("j", Lambda("k",
      RecNat(Nat, Var("j"), Var("k"),
        Lambda("prev", Lambda("sum",
          Add1(Var("sum")))))))
      .check(Ctx(), Arrow(Nat, Arrow(Nat, Nat))) ==
      Right(()))
  }

  "Module" can "define, claim and run exp" in {
    var m = Module()

    m.claim("three", Nat)
    m.define("three", Add1(Add1(Add1(Zero))))

    m.claim("+", Arrow(Nat, Arrow(Nat, Nat)))
    m.define("+",
      Lambda("n", Lambda("k",
        RecNat(Nat, Var("n"), Var("k"),
          Lambda("prev", Lambda("almost",
            Add1(Var("almost"))))))))

    m.run(Apply(Var("+"), Var("three")))

    m.run(Apply(Apply(Var("+"), Var("three")), Var("three")))
  }
}
