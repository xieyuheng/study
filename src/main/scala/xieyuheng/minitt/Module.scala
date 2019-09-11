package xieyuheng.minitt

import xieyuheng.minitt.pretty._

case class Module() {

  var top_list: List[Top] = List()

  def add_top(top: Top): Module = {
    top_list = top_list :+ top
    this
  }

  def declare(decl: Decl): Unit = {
    add_top(TopDecl(decl))
  }

  def env: Env = {
    var env: Env = EmptyEnv
    top_list.foreach {
      case TopDecl(decl) =>
        env = DeclEnv(decl, env)
      case _ => {}
    }
    env
  }

  def run(): Unit = {
    var env: Env = EmptyEnv
    top_list.foreach {
      case TopDecl(decl) =>
        env = DeclEnv(decl, env)
      case TopEval(exp) =>
        println(prettyExp(exp))
        val value = eval(exp, env)
        print("==> ")
        println(prettyValue(value))
        println()
      case TopEq(e1, e2) =>
        val v1 = eval(e1, env)
        val v2 = eval(e2, env)
        if (v1 != v2) {
          println(s"[eq! fail]")
          println(s"e1: ${prettyExp(e1)} ==> ${prettyValue(v1)}")
          println(s"e2: ${prettyExp(e2)} ==> ${prettyValue(v2)}")
          throw new Exception()
        }
      case TopNotEq(e1, e2) =>
        val v1 = eval(e1, env)
        val v2 = eval(e2, env)
        if (v1 == v2) {
          println(s"[not_eq! fail]")
          println(s"e1: ${prettyExp(e1)} ==> ${prettyValue(v1)}")
          println(s"e2: ${prettyExp(e2)} ==> ${prettyValue(v2)}")
          throw new Exception()
        }
    }
  }

  // old

  def let(pattern: Pattern, t: Exp, e: Exp): Unit = {
    declare(Let(pattern, t, e))
  }

  def letrec(pattern: Pattern, t: Exp, e: Exp): Unit = {
    declare(Letrec(pattern, t, e))
  }

  def import_all(module: Module): Unit = {
    top_list = top_list ++ module.top_list
  }

  def assert_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, this.env)
    val v2 = eval(e2, this.env)
    if (v1 != v2) {
      println(s"e1: ${prettyExp(e1)} ==> ${prettyValue(v1)}")
      println(s"e2: ${prettyExp(e2)} ==> ${prettyValue(v2)}")
      throw new Exception()
    }
  }

  def print_exp(exp: Exp): Value = {
    println(prettyExp(exp))
    val value = eval(exp, this.env)
    print("==> ")
    println(prettyValue(value))
    println()
    value
  }

}
