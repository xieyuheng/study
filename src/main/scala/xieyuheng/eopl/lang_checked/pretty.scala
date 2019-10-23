package xieyuheng.eopl.lang_checked

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
        s"let ${name} = ${pretty_exp(exp1)} ${pretty_exp(body)}"
      case Fn(name: String, arg_t: Type, body: Exp) =>
        s"(${name}: ${pretty_type(arg_t)}) => ${pretty_exp(body)}"
      case Ap(f: Fn, arg: Exp) =>
        s"{${pretty_exp(f)}}(${pretty_exp(arg)})"
      case Ap(target: Exp, arg: Exp) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
      case LetRec(fn_name, arg_name, arg_t, ret_t, fn_body, body) =>
        s"let rec ${fn_name} = (${arg_name}: ${pretty_type(arg_t)}): ${pretty_type(ret_t)} => ${pretty_exp(fn_body)}\n${pretty_exp(body)}"
      case LetRecMutual(map: Map[String, (String, Type, Type, Exp)], body: Exp) =>
        val s = map.map { case (fn_name, (arg_name, arg_t, ret_t, fn_body)) =>
          s"${fn_name} = (${arg_name}: ${pretty_type(arg_t)}): ${pretty_type(ret_t)} => ${pretty_exp(fn_body)}"
        }.mkString("\nand ")
        s"let rec ${s}\n${pretty_exp(body)}"
      case Sole() =>
        s"sole"
      case Do(exp1, body) =>
        s"do ${pretty_exp(exp1)} ${pretty_exp(body)}"
      case AssertEq(exp1, exp2) =>
        s"assert_eq(${pretty_exp(exp1)}, ${pretty_exp(exp2)})"
      case Show(exp1) =>
        s"println(${pretty_exp(exp1)})"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValNum(num: Int) =>
        num.toString
      case ValBool(bool: Boolean) =>
        bool.toString
      case ValFn(name, arg_t, body, env) =>
        s"(${name}: ${pretty_type(arg_t)}) => ${pretty_exp(body)}"
      case ValSole() =>
        s"sole"
    }
  }

  def pretty_type(t: Type): String = {
    t match {
      case TypeInt() =>
        s"int_t"
      case TypeBool() =>
        s"bool_t"
      case TypeSole() =>
        s"sole_t"
      case TypeArrow(arg_t: Type, ret_t: Type) =>
        s"(${pretty_type(arg_t)}) -> ${pretty_type(ret_t)}"
    }
  }
}
