package xieyuheng.eopl.lang_letrec

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name: String) =>
        name
      case Num(num: Int) =>
        num.toString
      case Diff(exp1: Exp, exp2: Exp) =>
        s"diff(${pretty_exp(exp1)}, ${pretty_exp(exp2)})"
      case ZeroP(exp1: Exp) =>
        s"zero_p(${pretty_exp(exp1)})"
      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        s"if ${pretty_exp(exp1)} then ${pretty_exp(exp2)} else ${pretty_exp(exp3)}"
      case Let(name: String, exp1: Exp, body: Exp) =>
        s"let ${name} = ${pretty_exp(exp1)} in ${pretty_exp(body)}"
      case Fn(name: String, body: Exp) =>
        s"(${name}) => ${pretty_exp(body)}"
      case Ap(f: Fn, arg: Exp) =>
        s"{${pretty_exp(f)}}(${pretty_exp(arg)})"
      case Ap(target: Exp, arg: Exp) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
      case LetRec(fn_name, arg_name, fn_body, body) =>
        s"let rec ${fn_name}(${arg_name}) = ${pretty_exp(fn_body)} in ${pretty_exp(body)}"
      case LetRecMutual(map: Map[String, (String, Exp)], body: Exp) =>
        val s = map.map { case (fn_name, (arg_name, fn_body)) =>
          s"${fn_name}(${arg_name}) = ${pretty_exp(fn_body)}"
        }.mkString(" and ")
        s"let rec ${s} in ${pretty_exp(body)}"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValNum(num: Int) =>
        num.toString
      case ValBool(bool: Boolean) =>
        bool.toString
      case ValFn(name, body, env) =>
        s"(${name}) => ${pretty_exp(body)}"
    }
  }

}
