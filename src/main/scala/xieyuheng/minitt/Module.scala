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
    var env: Env = EnvEmpty()
    top_list.foreach {
      case TopDecl(decl) =>
        env = EnvDecl(decl, env)
      case _ => {}
    }
    env
  }

  def run(): Unit = {
    var env: Env = EnvEmpty()
    top_list.foreach {
      case TopDecl(decl) =>
        env = EnvDecl(decl, env)
      case TopEval(exp) =>
        print_exp(exp)
      case TopEq(e1, e2) =>
        val v1 = eval(e1, env)
        val v2 = eval(e2, env)
        if (v1 != v2) {
          println(s"[eq! fail]")
          println(s"e1: ${prettyExp(e1)} ==> ${prettyVal(v1)}")
          println(s"e2: ${prettyExp(e2)} ==> ${prettyVal(v2)}")
          throw new Exception()
        }
      case TopNotEq(e1, e2) =>
        val v1 = eval(e1, env)
        val v2 = eval(e2, env)
        if (v1 == v2) {
          println(s"[not_eq! fail]")
          println(s"e1: ${prettyExp(e1)} ==> ${prettyVal(v1)}")
          println(s"e2: ${prettyExp(e2)} ==> ${prettyVal(v2)}")
          throw new Exception()
        }
    }
  }

  // old

  def let(pat: Pat, t: Exp, e: Exp): Unit = {
    declare(Let(pat, t, e))
  }

  def letrec(pat: Pat, t: Exp, e: Exp): Unit = {
    declare(Letrec(pat, t, e))
  }

  def import_all(module: Module): Unit = {
    top_list = top_list ++ module.top_list
  }

  def assert_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, this.env)
    val v2 = eval(e2, this.env)
    if (v1 != v2) {
      println(s"e1: ${prettyExp(e1)} ==> ${prettyVal(v1)}")
      println(s"e2: ${prettyExp(e2)} ==> ${prettyVal(v2)}")
      throw new Exception()
    }
  }

  def print_exp(exp: Exp): Unit = {
    println(prettyExp(exp))
    val value = eval(exp, this.env)
    print("==> ")
    println(prettyVal(value))
    println()
  }

}
