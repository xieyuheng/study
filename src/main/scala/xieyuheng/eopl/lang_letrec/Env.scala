package xieyuheng.eopl.lang_letrec

import scala.annotation.tailrec

sealed trait Env {

  @tailrec
  def lookup_val(name: String): Option[Val] = {
    val env = this
    env match {
      case EnvEmpty() =>
        None
      case EnvLet(name2: String, value: Val, rest: Env) =>
        if (name == name2) {
          Some(value)
        } else {
          rest.lookup_val(name)
        }
      case EnvLetRec(fn_name, arg_name, fn_body, rest) =>
        if (name == fn_name) {
          Some(ValFn(arg_name, fn_body, env))
        } else {
          rest.lookup_val(name)
        }
    }
  }

  def ext_let(name: String, value: Val): Env = {
    val rest = this
    EnvLet(name, value, rest)
  }

  def ext_fn(fn_name: String, arg_name: String, fn_body: Exp): Env = {
    val rest = this
    EnvLetRec(fn_name, arg_name, fn_body, rest)
  }

}

final case class EnvEmpty() extends Env
final case class EnvLet(name: String, value: Val, rest: Env) extends Env
final case class EnvLetRec(fn_name: String, arg_name: String, fn_body: Exp, rest: Env) extends Env
