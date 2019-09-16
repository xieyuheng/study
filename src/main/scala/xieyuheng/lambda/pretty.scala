package xieyuheng.lambda

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"${name}: ${pretty_exp(exp)};" }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) => name
      case Fn(name, body) =>
        s"${name} => ${pretty_exp(body)}"
      case Ap(fn, arg) =>
        s"${pretty_exp(fn)}(${pretty_exp(arg)})"
      case Block(decl, body) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, e) =>
        s"let ${name} = ${pretty_exp(e)}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        name
      case NeuAp(target: Neu, arg: Val) =>
        s"${pretty_neu(target)}(${pretty_val(arg)})"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case neu: Neu => pretty_neu(neu)
      case ValFn(name, body, env) =>
        // val map_str = pretty_env(env)
        // s"${name} => ${pretty_exp(body)} #env {${maybeln(map_str)}}"
        s"${name} => ${pretty_exp(body)}"
    }
  }

  def pretty_env(env: Env): String = {
    val delimiter = ";\n"
    env.map
      .map { case (name, value) => s"${name}: ${pretty_val(value)}" }
      .mkString(delimiter)
  }
}
