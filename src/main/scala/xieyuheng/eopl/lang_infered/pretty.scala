package xieyuheng.eopl.lang_infered

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
      case Fn(name: String, anno_arg_t: Option[Type], body: Exp) =>
        val s = pretty_anno_type(anno_arg_t)
        s"(${name}${s}) => ${pretty_exp(body)}"
      case Ap(f: Fn, arg: Exp) =>
        s"{${pretty_exp(f)}}(${pretty_exp(arg)})"
      case Ap(target: Exp, arg: Exp) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
      case LetRec(fn_name, arg_name, anno_arg_t, anno_ret_t, fn_body, body) =>
        val s1 = pretty_anno_type(anno_arg_t)
        val s2 = pretty_anno_type(anno_ret_t)
        s"let rec ${fn_name} = (${arg_name}${s1})${s2} => ${pretty_exp(fn_body)} ${pretty_exp(body)}"
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
      case ValFn(name, body, env) =>
        s"(${name}) => ${pretty_exp(body)}"
      case ValSole() =>
        s"sole"
    }
  }

  def pretty_anno_type(anno_type: Option[Type]): String = {
    anno_type match {
      case Some(t) =>
        s": ${pretty_type(t)}"
      case None =>
        s""
    }
  }

  def pretty_type(t: Type): String = {
    t match {
      case TypeVar(serial, aka) =>
        aka match {
          case Some(name) =>
            s"${name}#${serial}"
          case None =>
            s"#${serial}"
        }
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
