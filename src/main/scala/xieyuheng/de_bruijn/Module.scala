package xieyuheng.de_bruijn

import pretty._

case class Env(map: Map[String, Exp] = Map())

case class Module() {

  var top_list: List[Top] = List()

  def env_ext(env: Env, name: String, exp: Exp): Env = {
    Env(env.map + (name -> exp))
  }

  def eval_with_env(exp: Exp, env: Env): Exp = {
    val e = eval.expend_free_variables(exp, env, Set())
    eval.beta_eta_reduction(e)
  }

  def run(): Unit = {
    var env: Env = Env()
    var ctx: Ctx = Ctx()

    def expend_type_alias(t: Type, map: Map[String, Type]): Type = {
      t match {
        case TypeAtom(name) =>
          map.get(name) match {
            case Some(t) => t
            case None => TypeAtom(name)
          }
        case TypeArrow(arg_t, ret_t) =>
          TypeArrow(
            expend_type_alias(arg_t, map),
            expend_type_alias(ret_t, map))
      }
    }

    top_list.foreach {
      case TopDecl(DeclLet(name, t, exp)) =>
        check.check(ctx, exp, t) match {
          case Right(()) =>
            ctx = check.ctx_ext(ctx, name, t)
            val e = eval_with_env(exp, env)
            env = env_ext(env, name, e)
          case Left(err) =>
            println(err.msg)
            throw new Exception()
        }
      case TopShow(exp) =>
        show(exp, env)
      case TopStep(exp) =>
        step(exp, env)
      case TopWalkThrough(exp) =>
        walk_through(exp, env)
      case TopEq(e1, e2) =>
        assert_eq(e1, e2, env)
      case TopNotEq(e1, e2) =>
        assert_not_eq(e1, e2, env)
    }
  }

  def assert_not_eq(e1: Exp, e2: Exp, env: Env): Unit = {
    val v1 = eval_with_env(e1, env)
    val v2 = eval_with_env(e2, env)
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

  def assert_eq(e1: Exp, e2: Exp, env: Env): Unit = {
    val v1 = eval_with_env(e1, env)
    val v2 = eval_with_env(e2, env)
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

  def show(exp: Exp, env: Env): Unit = {
    val norm = eval_with_env(exp, env)
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_exp(norm)}")
    println()
  }

  def step(exp: Exp, env: Env): Unit = {
    val e = eval.expend_free_variables(exp, env, Set())
    val e2 = eval.beta_eta_step(e)
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_exp(e2)}")
    println()
  }

  def walk_through(exp: Exp, env: Env): Unit = {
    var e = eval.expend_free_variables(exp, env, Set())
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
