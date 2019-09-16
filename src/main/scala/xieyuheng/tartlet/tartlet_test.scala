package xieyuheng.tartlet

object tartlet_test extends App {
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

  val `eval should eval Fn` = {
    val exp = Fn("x", Fn("y", Var("y")))
    assert(exp.eval(Env()) ==
      Right(ValFn(EnvClo(Env(), "x", Fn("y", Var("y"))))))
  }

  val `it should eval Ap` = {
    val exp = Ap(Fn("x", Var("x")), Fn("x", Var("x")))
    assert(exp.eval(Env()) ==
      Right(ValFn(EnvClo(Env(), "x", Var("x")))))
  }

  val `Module can define` = {
    var m = Module()

    m.claim("three", Nat)
    m.define("three", Add1(Add1(Add1(Zero))))

    m.claim("+", Arrow(Nat, Arrow(Nat, Nat)))
    m.define("+",
      Fn("n", Fn("k",
        NatInd(
          Var("n"),
          Fn("_", Nat),
          Var("k"),
          Fn("prev", Fn("almost",
            Add1(Var("almost"))))))))

    m.run(Var("three"))
    // The(Nat,Add1(Add1(Add1(Zero))))

    m.run(Ap(Var("+"), Var("three")))
    // The(Pi(_*,Nat,Nat),Fn(_*,Add1(Add1(Add1(Var(_*))))))

    m.run(Ap(Ap(Var("+"), Var("three")), Var("three")))
    // The(Nat,Add1(Add1(Add1(Add1(Add1(Add1(Zero)))))))


    // m.claim("three", Nat)
    // m.define("three", add1(add1(add1(zero))))

    // claim("+", Nat ->: Nat ->: Nat)
    // define_fn("+") ("n", "k") {
    //   ind_Nat("n", fn("_") { Nat }, "k",
    //     fn("prev", "almost") { add1("almost") })}

    // run("three")
    // run("+" ap "three")
    // run("+" ap "three" ap "three")
  }
}
