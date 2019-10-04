package xieyuheng.simple

import scala.annotation.tailrec

object practical {

  sealed trait Exp
  final case class Var(name: String, type_annotation: Option[Type]) extends Exp
  final case class Ap(target: Exp, arg: Exp) extends Exp
  final case class Fn(arg_name: String, arg_type_annotation: Option[Type], body: Exp) extends Exp

  sealed trait Type
  final case class TypeAtom(name: String) extends Type
  final case class TypeArrow(ante: Type, succ: Type) extends Type

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
  def beta_reduction(exp: Exp): Exp = {
    val exp2 = beta_step(exp)
    if (exp == exp2) {
      exp
    } else {
      beta_reduction(exp2)
    }
  }

  object practical_test extends App {
    val f = Fn("x", None, Var("x", None))
    val a = Var("a", None)
    println(beta_reduction(Ap(f, a)))
  }
  // def eta_step()

  // def eta_reduction()

  // def beta_eta_reduction

}
