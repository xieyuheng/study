package xieyuheng.tartlet

import eval._
import check._
import pretty._
import readback._

case class Module() {

  var top_list: List[Top] = List()

  def run(): Unit = {
    var ctx: Ctx = Ctx()
    top_list.foreach {
      case TopDecl(DeclLet(name, t, e)) =>
        ctx = claim(ctx, name, t)
        ctx = define(ctx, name, e)
      case TopShow(exp) =>
        show(ctx, exp)
      case TopEq(e1, e2) =>
        assert_eq(ctx, e1, e2)
      case TopNotEq(e1, e2) =>
        assert_not_eq(ctx, e1, e2)
      case _ => {}
    }
  }

  def claim(ctx: Ctx, name: String, t: Exp): Ctx = {
    if (ctx.lookup_type(name).isDefined) {
      println(s"name: ${name} is alreay defined")
      throw new Exception()
    } else {
      check(t, ctx, ValUniverse()) match {
        case Right(t_checked) =>
          val t_val = eval_unwrap(t_checked, ctx.to_env)
          ctx.ext(name, Bind(t_val))
        case Left(err) =>
          println(s"type check fail, name: ${name}, error: ${err.msg}")
          throw new Exception()
      }
    }
  }

  def define(ctx: Ctx, name: String, exp: Exp): Ctx = {
    ctx.lookup_den(name) match {
      case Some(Bind(t_val)) =>
        check(exp, ctx, t_val) match {
          case Right(exp) =>
            val value = eval_unwrap(exp, ctx.to_env)
            ctx.ext(name, Def(t_val, value))
          case Left(err) =>
            println(s"type check fail for name: ${name}, error: ${err.msg}")
            throw new Exception()
        }
      case Some(Def(t_val, value)) =>
        println(s"name: ${name} is already defined, type: ${t_val}, value: ${value}")
        throw new Exception()
      case None =>
        println(s"name: ${name} is not claimed before define")
        throw new Exception()
    }
  }

  def assert_not_eq(ctx: Ctx, e1: Exp, e2: Exp): Unit = {
    val env = ctx.to_env
    val v1 = eval_unwrap(e1, env)
    val v2 = eval_unwrap(e2, env)
    // val n1 = readback_val(v1, Set())
    // val n2 = readback_val(v2, Set())
    if (v1 == v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be not equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      // println(s"=== ${pretty_exp(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      // println(s"=== ${pretty_exp(n2)}")
      throw new Exception()
    }
  }

  def assert_eq(ctx: Ctx, e1: Exp, e2: Exp): Unit = {
    val env = ctx.to_env
    val v1 = eval_unwrap(e1, env)
    val v2 = eval_unwrap(e2, env)
    // val n1 = readback_val(v1, Set())
    // val n2 = readback_val(v2, Set())
    if (v1 != v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      // println(s"=== ${pretty_exp(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      // println(s"=== ${pretty_exp(n2)}")
      throw new Exception()
    }
  }

  def show(ctx: Ctx, exp: Exp): Unit = {
    val env = ctx.to_env
    val value = eval_unwrap(exp, env)
    val result = for {
      the <- infer(exp, ctx)
      t_val <- eval(the.t, env)
      value <- eval(exp, env)
      norm <- readback_val(value, t_val, ctx)
    } yield The(the.t, norm)

    result match {
      case Right(the_exp) =>
        println(s">>> ${pretty_exp(exp)}")
        println(s"=== ${pretty_val(value)}")
        println(s"=== ${pretty_exp(the_exp)}")
        println()
      case Left(err) =>
        println(s"error: ${err.msg}")
        throw new Exception()
    }
  }

}
