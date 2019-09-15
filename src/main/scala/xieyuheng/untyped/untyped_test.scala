package xieyuheng.untyped

object untyped_test extends App {
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
    assert(eval(exp, Env()) ==
      Right(Closure(Env(), "x", Lambda("y", Var("y")))))
  }

  val `it should eval Ap` = {
    val exp = Ap(Lambda("x", Var("x")), Lambda("x", Var("x")))
    assert(eval(exp, Env()) ==
      Right(Closure(Env(), "x", Var("x"))))
  }

  val `readback should readback normal form` = {
    val exp = Ap(
      Lambda("x", Lambda("y", Ap(Var("x"), Var("y")))),
      Lambda("x", Var("x")))
    for {
      value <- eval(exp, Env())
      norm <- readback(value, Set())
    } yield assert(norm == Lambda("y", Var("y")))
  }

  val `Module can define and run exp` = {
    val m = Module()
    m.define("id", Lambda("x", Var("x")))
    assert(m.run(Var("id")) == Right(Lambda("x", Var("x"))))
    assert(m.run(Lambda("x", Var("x"))) == Right(Lambda("x", Var("x"))))
  }
}
