package xieyuheng.systemt

object test extends App {
  val `freshen should generate new name not used` = {
    val used_names = Set("x", "x*")
    val fresh_name = util.freshen(used_names, "x")
    assert(!used_names.contains(fresh_name))
  }

  val `it should add * to the end of name` = {
    val used_names = Set("x", "x*")
    val fresh_name = util.freshen(used_names, "x")
    assert(fresh_name == "x**")
  }

  val `eval should eval Lambda` = {
    val exp = Lambda("x", Lambda("y", Var("y")))
    assert(exp.eval(Env()) ==
      Right(Closure(Env(), "x", Lambda("y", Var("y")))))
  }

  val `it should eval Ap` = {
    val exp = Ap(Lambda("x", Var("x")), Lambda("x", Var("x")))
    assert(exp.eval(Env()) ==
      Right(Closure(Env(), "x", Var("x"))))
  }

  val `infer should infer type of Var` = {
    val ctx = Ctx().ext("x", Nat)

    assert(Var("x").infer(ctx) == Right(Nat))
  }

  val `check should` = {
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

  val `Module can define, claim and run exp` = {
    var m = Module()

    m.claim("three", Nat)
    m.define("three", Add1(Add1(Add1(Zero))))

    m.claim("+", Arrow(Nat, Arrow(Nat, Nat)))
    m.define("+",
      Lambda("n", Lambda("k",
        RecNat(Nat, Var("n"), Var("k"),
          Lambda("prev", Lambda("almost",
            Add1(Var("almost"))))))))

    m.run(Ap(Var("+"), Var("three")))

    m.run(Ap(Ap(Var("+"), Var("three")), Var("three")))
  }
}
