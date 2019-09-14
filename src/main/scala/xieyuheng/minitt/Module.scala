package xieyuheng.minitt

import xieyuheng.minitt.check._
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

  def check(): Unit = {
    var env: Env = EnvEmpty()
    var ctx: Ctx = CtxEmpty()
    top_list.foreach {
      case TopDecl(decl) =>
        check_decl(0, env, ctx, decl) match {
          case Right(ctx2) =>
            ctx = ctx2
          case Left(err) =>
            println(s"${err.msg}")
            throw new Exception()
        }
        env = EnvDecl(decl, env)
      case _ =>
    }
  }

  def run(): Unit = {
    var env: Env = EnvEmpty()
    top_list.foreach {
      case TopDecl(decl) =>
        env = EnvDecl(decl, env)
      case TopEval(exp) =>
        print_exp(exp)
      case TopEq(e1, e2) =>
        assert_eq(e1)(e2)
      case TopNotEq(e1, e2) =>
        assert_not_eq(e1)(e2)
    }
  }

  def let(pat: Pat, t: Exp, e: Exp): Unit = {
    declare(DeclLet(pat, t, e))
  }

  def letrec(pat: Pat, t: Exp, e: Exp): Unit = {
    declare(DeclLetrec(pat, t, e))
  }

  def import_all(module: Module): Unit = {
    top_list = top_list ++ module.top_list
  }

  def assert_not_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, this.env)
    val v2 = eval(e2, this.env)
    if (v1 == v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be not equal")
      println(s">>> ${prettyExp(e1)}")
      println(s"=== ${prettyVal(v1)}")
      println(s">>> ${prettyExp(e2)}")
      println(s"=== ${prettyVal(v2)}")
      throw new Exception()
    }
  }

  def assert_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, this.env)
    val v2 = eval(e2, this.env)
    if (v1 != v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be equal")
      println(s">>> ${prettyExp(e1)}")
      println(s"=== ${prettyVal(v1)}")
      println(s">>> ${prettyExp(e2)}")
      println(s"=== ${prettyVal(v2)}")
      throw new Exception()
    }
  }

  def print_exp(exp: Exp): Unit = {
    print(">>> ")
    println(prettyExp(exp))
    val value = eval(exp, this.env)
    print("=== ")
    println(prettyVal(value))
    println()
  }

}
