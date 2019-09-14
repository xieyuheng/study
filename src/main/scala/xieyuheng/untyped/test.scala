package xieyuheng.untyped

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

  val `it should eval Apply` = {
    val exp = Apply(Lambda("x", Var("x")), Lambda("x", Var("x")))
    assert(exp.eval(Env()) ==
      Right(Closure(Env(), "x", Var("x"))))
  }

  val `readback should readback normal form` = {
    for {
      value <- Apply(
        Lambda("x", Lambda("y", Apply(Var("x"), Var("y")))),
        Lambda("x", Var("x")))
      .eval(Env())
      norm <- value.readback(Set())
    } yield assert(norm == Lambda("y", Var("y")))
  }

  val `Module can define and run exp` = {
    val m = Module()
    m.define("id", Lambda("x", Var("x")))
    assert(m.run(Var("id")) == Right(Lambda("x", Var("x"))))
    assert(m.run(Lambda("x", Var("x"))) == Right(Lambda("x", Var("x"))))
  }
}
