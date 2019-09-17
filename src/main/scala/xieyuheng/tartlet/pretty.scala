package xieyuheng.tartlet

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"${name}: ${pretty_exp(exp)};" }

  def pretty_exp(exp: Exp): String = {
    exp.toString
    // exp match {
    //   case Var(name) => name
    //   case Fn(name, body) =>
    //     s"${name} => ${pretty_exp(body)}"
    //   case Ap(fn, arg) =>
    //     s"${pretty_exp(fn)}(${pretty_exp(arg)})"
    //   case The(t: Type, e: Exp) =>
    //     s"the(${pretty_type(t)}, ${pretty_exp(e)})"
    //   case Zero()() =>
    //     s"zero"
    //   case Succ(prev: Exp) =>
    //     s"succ(${pretty_exp(prev)})"
    //   case NatRec(t: Type, target: Exp, base: Exp, step: Exp) =>
    //     s"nat_rec(${pretty_type(t)}, ${pretty_exp(target)}, ${pretty_exp(base)}, ${pretty_exp(step)})"
    //   case Block(decl, body) =>
    //     s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    // }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, t, e) =>
        s"let ${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu.toString
    // neu match {
    //   case NeuVar(name: String) =>
    //     name
    //   case NeuAp(target: Neu, arg: TheVal) =>
    //     s"${pretty_neu(target)}(${pretty_the_val(arg)})"
    //   case NeuNatRec(t: Type, target: Neu, base: TheVal, step: TheVal) =>
    //     s"nat_rec(${pretty_type(t)}, ${pretty_neu(target)}, ${pretty_the_val(base)}, ${pretty_the_val(step)})"
    // }
  }

  def pretty_the_val(the: TheVal): String = {
    s"the(${pretty_val(the.t)}, ${pretty_val(the.value)})"
  }

  def pretty_val(value: Val): String = {
    value.toString
    // value match {
    //   case TheNeu(t: Type, neu: Neu) =>
    //     s"the(${pretty_type(t)}, ${pretty_neu(neu)})"
    //   case ValSucc(prev: Val) =>
    //     s"succ(${pretty_val(prev)})"
    //   case ValZero()() =>
    //     s"zero"
    //   case ValFn(name, body, env) =>
    //     // val map_str = pretty_env(env)
    //     // s"${name} => ${pretty_exp(body)} #env {${maybeln(map_str)}}"
    //     s"${name} => ${pretty_exp(body)}"
    // }
  }

  // def pretty_env(env: Env): String = {
  //   val delimiter = ";\n"
  //   env.map
  //     .map { case (name, value) => s"${name}: ${pretty_val(value)}" }
  //     .mkString(delimiter)
  // }
}
