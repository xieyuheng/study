package xieyuheng.eopl.lang_nameless

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
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValNum(num: Int) =>
        num.toString
      case ValBool(bool: Boolean) =>
        bool.toString
      case ValFn(name, body, env) =>
        s"(${name}) => ${pretty_idx(body)}"
    }
  }

  def pretty_idx(idx: Idx): String = {
    idx match {
      case IdxVar(name, index) =>
        s"${index}#${name}"
      case IdxNum(num) =>
        num.toString
      case IdxDiff(idx1, idx2) =>
        s"diff(${pretty_idx(idx1)}, ${pretty_idx(idx2)})"
      case IdxZeroP(idx1) =>
        s"zero_p(${pretty_idx(idx1)})"
      case IdxIf(idx1, idx2, idx3) =>
        s"if ${pretty_idx(idx1)} then ${pretty_idx(idx2)} else ${pretty_idx(idx3)}"
      case IdxLet(name, idx1, body) =>
        s"let ${name} = ${pretty_idx(idx1)} in ${pretty_idx(body)}"
      case IdxFn(name, body) =>
        s"(${name}) => ${pretty_idx(body)}"
      case IdxAp(f: IdxFn, arg) =>
        s"{${pretty_idx(f)}}(${pretty_idx(arg)})"
      case IdxAp(target, arg) =>
        s"${pretty_idx(target)}(${pretty_idx(arg)})"
    }
  }

}
