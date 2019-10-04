package xieyuheng.simple

import pretty._

case class Module() {

  var top_list: List[Top] = List()

  def eval_with_global(exp: Exp, global: Map[String, Exp]): Exp = {
    val e = eval.expend_global_variables(exp, global, Set())
    eval.beta_eta_reduction(e)
  }

  def run(): Unit = {
    var global: Map[String, Exp] = Map()

    top_list.foreach {
      case TopDecl(DeclLet(name, exp)) =>
        val e = eval_with_global(exp, global)
        global = global + (name -> e)
      case TopShow(exp) =>
        show(exp, global)
      case TopStep(exp) =>
        step(exp, global)
      case TopWalkThrough(exp) =>
        walk_through(exp, global)
      case TopEq(e1, e2) =>
        assert_eq(e1, e2, global)
      case TopNotEq(e1, e2) =>
        assert_not_eq(e1, e2, global)
      case _ => {}
    }
  }

  def assert_not_eq(e1: Exp, e2: Exp, global: Map[String, Exp]): Unit = {
    val v1 = eval_with_global(e1, global)
    val v2 = eval_with_global(e2, global)
    if (v1 == v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be not equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_exp(v1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_exp(v2)}")
      throw new Exception()
    }
  }

  def assert_eq(e1: Exp, e2: Exp, global: Map[String, Exp]): Unit = {
    val v1 = eval_with_global(e1, global)
    val v2 = eval_with_global(e2, global)
    if (v1 != v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_exp(v1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_exp(v2)}")
      throw new Exception()
    }
  }

  def show(exp: Exp, global: Map[String, Exp]): Unit = {
    val norm = eval_with_global(exp, global)
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_exp(norm)}")
    println()
  }

  def step(exp: Exp, global: Map[String, Exp]): Unit = {
    val e = eval.expend_global_variables(exp, global, Set())
    val e2 = eval.beta_eta_step(e)
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_exp(e2)}")
    println()
  }

  def walk_through(exp: Exp, global: Map[String, Exp]): Unit = {
    var e = eval.expend_global_variables(exp, global, Set())
    println(s">>> ${pretty_exp(exp)}")
    var u = eval.beta_eta_step(e)
    while (e != u) {
      println(s"=== ${pretty_exp(u)}")
      e = u
      u = eval.beta_eta_step(u)
    }
    println()
  }

}
