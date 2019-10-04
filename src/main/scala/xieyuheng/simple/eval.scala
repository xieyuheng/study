package xieyuheng.simple

import scala.annotation.tailrec

object eval {

  def free_variables(exp: Exp): Set[String] = {
    exp match {
      case Var(name, type_annotation) =>
        Set(name)
      case Ap(target, arg) =>
        free_variables(target) ++ free_variables(arg)
      case Fn(arg_name, arg_type_annotation, body) =>
        free_variables(body) - arg_name
    }
  }

  def free_variable_p(name: String, exp: Exp): Boolean = {
    free_variables(exp).contains(name)
  }

  def subst(body: Exp, arg_name: String, arg: Exp): Exp = {
    body match {
      case Var(name: String, type_annotation: Option[Type]) =>
        if (arg_name == name) {
          arg
        } else {
          body
        }
      case Ap(target: Exp, arg2: Exp) =>
        Ap(
          subst(target, arg_name, arg),
          subst(arg2, arg_name, arg))
      case Fn(arg_name2: String, arg_type_annotation: Option[Type], body2: Exp) =>
        if (arg_name == arg_name2) {
          body
        } else {
          Fn(
            arg_name2, arg_type_annotation,
            subst(body2, arg_name, arg))
        }
    }
  }

  def beta_step(exp: Exp): Exp = {
    exp match {
      case Var(name: String, type_annotation: Option[Type]) =>
        exp
      case Ap(Fn(arg_name: String, arg_type_annotation: Option[Type], body: Exp), arg: Exp) =>
        subst(body, arg_name, arg)
      case Ap(target: Exp, arg: Exp) =>
        val target2 = beta_step(target)
        if (target2 == target) {
          val arg2 = beta_step(arg)
          Ap(target, arg2)
        } else {
          Ap(target2, arg)
        }
      case Fn(arg_name: String, arg_type_annotation: Option[Type], body: Exp) =>
        Fn(arg_name: String, arg_type_annotation: Option[Type], beta_step(body))
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
      case Var(name: String, type_annotation: Option[Type]) =>
        exp
      case Ap(target: Exp, arg: Exp) =>
        val target2 = eta_step(target)
        if (target2 == target) {
          val arg2 = eta_step(arg)
          Ap(target, arg2)
        } else {
          Ap(target2, arg)
        }
      case Fn(arg_name, arg_type_annotation, Ap(target, Var(name, type_annotation))) =>
        if (arg_name == name && free_variable_p(name, target)) {
          target
        } else {
          Fn(arg_name, arg_type_annotation, Ap(eta_step(target), Var(name, type_annotation)))
        }
      case Fn(arg_name, arg_type_annotation, body) =>
        Fn(arg_name, arg_type_annotation, eta_step(body))
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

}
