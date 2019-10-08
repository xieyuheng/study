package xieyuheng.de_bruijn

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"${name}: ${pretty_exp(exp)};" }

  def pretty_type(t: Type): String = {
    t match {
      case TypeAtom(name) =>
        name
      case TypeArrow(arg_t, ret_t) =>
        s"(${pretty_type(arg_t)}) -> ${pretty_type(ret_t)}"
    }
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) =>
        name
      case Fn(name, arg_t, body) =>
        s"(${name}: ${pretty_type(arg_t)}) => ${pretty_exp(body)}"
      case Ap(fn: Fn, arg) =>
        s"{${pretty_exp(fn)}}(${pretty_exp(arg)})"
      case Ap(target, arg) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, t, e) =>
        s"let ${name}: ${pretty_type(t)} = ${pretty_exp(e)}"
    }
  }

}
