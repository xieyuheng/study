package xieyuheng.eopl.lang_infered

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
      case EnvLetRecMutual(map, rest) =>
        map.get(name) match {
          case Some((arg_name, fn_body)) =>
            Some(ValFn(arg_name, fn_body, env))
          case None =>
            rest.lookup_val(name)
        }
    }
  }

  def ext_let(name: String, value: Val): Env = {
    val rest = this
    EnvLet(name, value, rest)
  }

  def ext_let_rec(fn_name: String, arg_name: String, fn_body: Exp): Env = {
    val rest = this
    EnvLetRec(fn_name, arg_name, fn_body, rest)
  }

  def ext_let_rec_mutual(map: Map[String, (String, Option[Type], Option[Type], Exp)]): Env = {
    val rest = this
    val runtime_map: Map[String, (String, Exp)] = map.map {
      case (fn_name, (arg_name, anno_arg_t, anno_ret_t, body)) =>
        (fn_name, (arg_name, body))
    }
    EnvLetRecMutual(runtime_map, rest)
  }

}

final case class EnvEmpty() extends Env
final case class EnvLet(name: String, value: Val, rest: Env) extends Env
final case class EnvLetRec(fn_name: String, arg_name: String, fn_body: Exp, rest: Env) extends Env
final case class EnvLetRecMutual(map: Map[String, (String, Exp)], rest: Env) extends Env
