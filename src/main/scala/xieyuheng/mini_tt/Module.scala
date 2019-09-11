package xieyuheng.mini_tt

import xieyuheng.mini_tt.pretty._

case class Module() {
  var env: Env = EmptyEnv

  def declare(decl: Decl): Unit = {
    env = DeclEnv(decl, env)
  }

  def let(pattern: Pattern, t: Exp, e: Exp): Unit = {
    declare(Let(pattern, t, e))
  }

  def letrec(pattern: Pattern, t: Exp, e: Exp): Unit = {
    declare(Letrec(pattern, t, e))
  }

  def import_all(module: Module): Unit = {
    env = Env.append(module.env, env)
  }

  def run(exp: Exp): Value = eval(exp, env)

  def assert_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = run(e1)
    val v2 = run(e2)
    if (v1 != v2) {
      println(s"e1: ${prettyExp(e1)} ==> ${prettyValue(v1)}")
      println(s"e2: ${prettyExp(e2)} ==> ${prettyValue(v2)}")
      throw new Exception()
    }
  }

  def run_print(exp: Exp): Value = {
    println(prettyExp(exp))
    val value = run(exp)
    print("==> ")
    println(prettyValue(value))
    println()
    value
  }

}
