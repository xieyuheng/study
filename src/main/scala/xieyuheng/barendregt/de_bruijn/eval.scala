package xieyuheng.barendregt.de_bruijn

import scala.annotation.tailrec

object eval {

  def free_variables(exp: Exp): Set[String] = {
    exp match {
      case Var(name) =>
        Set(name)
      case Ap(target, arg) =>
        free_variables(target) ++ free_variables(arg)
      case Fn(arg_name, arg_t, body) =>
        free_variables(body) - arg_name
      case atom: Atom =>
        Set()
    }
  }

  def free_variable_p(name: String, exp: Exp): Boolean = {
    free_variables(exp).contains(name)
  }

  @tailrec
  def gen_name_for_arg(arg_name: String, arg: Exp): String = {
    if (free_variable_p(arg_name, arg)) {
      gen_name_for_arg(arg_name ++ "*", arg)
    } else {
      arg_name
    }
  }

  def subst(body: Exp, arg_name: String, arg: Exp): Exp = {
    body match {
      case Var(name: String) =>
        if (arg_name == name) {
          arg
        } else {
          Var(name)
        }
      case Ap(target: Exp, arg2: Exp) =>
        Ap(
          subst(target, arg_name, arg),
          subst(arg2, arg_name, arg))
      case Fn(arg_name2: String, arg_t: Type, body2: Exp) =>
        if (arg_name2 == arg_name) {
          Fn(arg_name2, arg_t, body2)
        } else {
          val new_name = gen_name_for_arg(arg_name2, arg)
          val new_body = subst(body2, arg_name2, Var(new_name))
          Fn(new_name, arg_t, subst(new_body, arg_name, arg))
        }
      case atom: Atom =>
        atom
    }
  }

  def beta_step(exp: Exp): Exp = {
    exp match {
      case Var(name: String) =>
        exp
      case Ap(Fn(arg_name: String, arg_t: Type, body: Exp), arg: Exp) =>
        subst(body, arg_name, arg)
      case Ap(target: Exp, arg: Exp) =>
        val target2 = beta_step(target)
        if (target2 == target) {
          val arg2 = beta_step(arg)
          Ap(target, arg2)
        } else {
          Ap(target2, arg)
        }
      case Fn(arg_name: String, arg_t: Type, body: Exp) =>
        Fn(arg_name, arg_t, beta_step(body))
      case atom: Atom =>
        atom
    }
  }

  @tailrec
  def reduction_from_step(step: Exp => Exp, exp: Exp): Exp = {
    val exp2 = step(exp)
    if (exp == exp2) {
      exp
    } else {
      reduction_from_step(step, exp2)
    }
  }

  def beta_reduction(exp: Exp): Exp = {
    reduction_from_step(beta_step, exp)
  }

  def eta_step(exp: Exp): Exp = {
    exp match {
      case Var(name: String) =>
        exp
      case Ap(target: Exp, arg: Exp) =>
        val target2 = eta_step(target)
        if (target2 == target) {
          val arg2 = eta_step(arg)
          Ap(target, arg2)
        } else {
          Ap(target2, arg)
        }
      case Fn(arg_name, arg_t, Ap(target, Var(name))) =>
        if (arg_name == name && free_variable_p(name, target)) {
          target
        } else {
          Fn(arg_name, arg_t, Ap(eta_step(target), Var(name)))
        }
      case Fn(arg_name, arg_t, body) =>
        Fn(arg_name, arg_t, eta_step(body))
      case atom: Atom =>
        atom
    }
  }

  def eta_reduction(exp: Exp): Exp = {
    reduction_from_step(eta_step, exp)
  }

  def beta_eta_step(exp: Exp): Exp = {
    val exp2 = beta_step(exp)
    if (exp2 == exp) {
      eta_step(exp)
    } else {
      exp2
    }
  }

  def beta_eta_reduction(exp: Exp): Exp = {
    reduction_from_step(beta_eta_step, exp)
  }

  def expend_free_variables(
    exp: Exp,
    env: Env,
    bound_variables: Set[String],
  ): Exp = {
    env.map.foldLeft(exp) { case (exp, (name, arg)) =>
      subst(exp, name, arg)
    }
  }

}
