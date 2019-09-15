package xieyuheng.lambda

import pretty._
import readback._

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
    var env: Env = Env()
    top_list.foreach {
      case TopDecl(DeclLet(name, exp)) =>
        env = env.ext(name, eval(exp, env))
      case _ => {}
    }
    env
  }

  def run(): Unit = {
    var env: Env = Env()
    top_list.foreach {
      case TopDecl(DeclLet(name, exp)) =>
        env = env.ext(name, eval(exp, env))
      case TopEval(exp) =>
        eval_print(exp)
      case TopEq(e1, e2) =>
        assert_eq(e1)(e2)
      case TopNotEq(e1, e2) =>
        assert_not_eq(e1)(e2)
      case _ => {}
    }
  }

  def assert_not_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, env)
    val v2 = eval(e2, env)
    val n1 = readback_val(v1, Set())
    val n2 = readback_val(v2, Set())
    if (v1 == v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be not equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      println(s"=== ${pretty_exp(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      println(s"=== ${pretty_exp(n2)}")
      throw new Exception()
    }
  }

  def assert_eq(e1: Exp)(e2: Exp): Unit = {
    val v1 = eval(e1, env)
    val v2 = eval(e2, env)
    val n1 = readback_val(v1, Set())
    val n2 = readback_val(v2, Set())
    if (v1 != v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      println(s"=== ${pretty_exp(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      println(s"=== ${pretty_exp(n2)}")
      throw new Exception()
    }
  }

  def eval_print(exp: Exp): Unit = {
    val value = eval(exp, env)
    val norm = readback_val(value, Set())
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_val(value)}")
    println(s"=== ${pretty_exp(norm)}")
    println()
  }

}
