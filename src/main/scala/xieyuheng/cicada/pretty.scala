package xieyuheng.cicada

import collection.immutable.ListMap

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name: String) =>
        s"${name}"
      case Type() =>
        s"type"
      case Pi(arg_map: ListMap[String, Exp], ret_type: Exp) =>
        var s = arg_map.map {
          case (name, exp) => s"given ${name} : ${pretty_exp(exp)}\n"
        }.mkString("")
        s = s + s"conclude ${pretty_exp(ret_type)}\n"
        s"{${maybe_ln(s)}}"
      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        var s = arg_map.map {
          case (name, exp) => s"given ${name} : ${pretty_exp(exp)}\n"
        }.mkString("")
        s = s + s"return ${pretty_exp(body)}\n"
        s"{${maybe_ln(s)}}"
      case Ap(target: Exp, arg_list: List[Exp]) =>
        val args = arg_list.map {
          case exp => pretty_exp(target)
        }.mkString(", ")
        s"${pretty_exp(target)}(${args})"
      case Cl(type_map: ListMap[String, Exp]) =>
        var s = type_map.map {
          case (name, exp) => s"given ${name} : ${pretty_exp(exp)}\n"
        }.mkString("")
        s"class {${maybe_ln(s)}}"
      case Obj(val_map: ListMap[String, Exp]) =>
        var s = val_map.map {
          case (name, exp) => s"let ${name} = ${pretty_exp(exp)}\n"
        }.mkString("")
        s"object {${maybe_ln(s)}}"
      case Dot(target: Exp, field: String) =>
        s"${pretty_exp(target)}.${field}"
      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        var s = let_map.map {
          case (name, exp) => s"let ${name} = ${pretty_exp(exp)}\n"
        }.mkString("")
        s = s + s"return ${pretty_exp(body)}\n"
        s"{${maybe_ln(s)}}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        s"${name}"
      case NeuAp(target: Neu, arg_list: List[Val]) =>
        val args = arg_list.map {
          case value => pretty_val(value)
        }.mkString(", ")
        s"${pretty_neu(target)}(${args})"
      case NeuDot(target: Neu, field: String) =>
        s"${pretty_neu(target)}.${field}"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValType() =>
        s"type"
      case ValPi(arg_map: ListMap[String, Exp], ret_type: Exp, env: Env) =>
        var s = arg_map.map {
          case (name, exp) => s"given ${name} : ${pretty_exp(exp)}\n"
        }.mkString("")
        s = s + s"conclude ${pretty_exp(ret_type)}\n"
        s"{${maybe_ln(s)}}"
      case ValFn(arg_map: ListMap[String, Exp], body: Exp, env: Env) =>
        var s = arg_map.map {
          case (name, exp) => s"given ${name} : ${pretty_exp(exp)}\n"
        }.mkString("")
        s = s + s"return ${pretty_exp(body)}\n"
        s"{${maybe_ln(s)}}"
      case ValCl(type_map: ListMap[String, Exp], env: Env) =>
        var s = type_map.map {
          case (name, exp) => s"let ${name} = ${pretty_exp(exp)}\n"
        }.mkString("")
        s"class {${maybe_ln(s)}}"
      case ValObj(val_map: ListMap[String, Val]) =>
        var s = val_map.map {
          case (name, value) => s"let ${name} = ${pretty_val(value)}\n"
        }.mkString("")
        s"object {${maybe_ln(s)}}"
      case neu: Neu => pretty_neu(neu)
    }
  }
}
