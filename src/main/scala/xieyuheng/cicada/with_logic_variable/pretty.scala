package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map_with_delimiter(
    map: MultiMap[String, Exp],
    delimiter: String,
  ): String = {
    map.entries
      .map { case (name, exp) => s"${name}: ${pretty_exp(exp)}" }
      .mkString(delimiter)
  }

  def pretty_exp_map(map: MultiMap[String, Exp]): String = {
    pretty_exp_map_with_delimiter(map, "\n")
  }

  def pretty_exp_args(map: MultiMap[String, Exp]): String = {
    pretty_exp_map_with_delimiter(map, ", ")
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case The(t) =>
        s"the(${pretty_exp(t)})"
      case Choice(target, map) =>
        val mapString = maybeln(pretty_exp_map(map))
        s"${pretty_exp(target)} case {${mapString}}"
      case Dot(target, fieldName) =>
        s"${pretty_exp(target)}.${fieldName}"
      case Pi(args, ret) =>
        s"pi (${pretty_exp_args(args)}): ${pretty_exp(ret)}"
      case Fn(args, ret, body) =>
        val bodyString = maybeln(pretty_exp(body))
        s"fn (${pretty_exp_args(args)}): ${pretty_exp(ret)} = {${bodyString}}"
      case Ap(target, args) =>
        s"${pretty_exp(target)}(${pretty_exp_args(args)})"
    }
  }

  def pretty_valMapWithDelimiter(
    map: ListMap[String, Val],
    bind: Bind,
    delimiter: String,
  ): String = {
    walk.deepOnMap(map, bind)
      .map { case (name, value) => s"${name}: ${pretty_val(value)}" }
      .mkString(delimiter)
  }

  def pretty_valMap(map: ListMap[String, Val], bind: Bind): String = {
    pretty_valMapWithDelimiter(map, bind, "\n")
  }

  def pretty_valArgs(map: ListMap[String, Val], bind: Bind): String = {
    pretty_valMapWithDelimiter(map, bind, ", ")
  }

  def pretty_neu(neutral: Neu, bind: Bind): String = {
    neutral match {
      case VarNeu(name) =>
        name
      case ChoiceNeu(target, map) =>
        val mapString = maybeln(pretty_valMap(map, bind))
        s"${pretty_neu(target, bind)} case {${mapString}}"
      case DotNeu(target, fieldName) =>
        s"${pretty_neu(target, bind)}.${fieldName}"
      case ApNeu(target, args) =>
        s"${pretty_neu(target, bind)}(${pretty_valArgs(args, bind)})"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case TypeOfType(id) =>
        s"type(${id})"
      case ValOfType(id, t) =>
        s"the(${id}, ${pretty_val(t)})"
      case SumTypeVal(name, map, memberNames, bind) =>
        val mapString = maybeln(pretty_valMap(map, bind))
        s"${name} {${mapString}}"
      case MemberTypeVal(name, map, superName, bind) =>
        val mapString = maybeln(pretty_valMap(map, bind))
        s"${name} {${mapString}}"
      case PiVal(args, ret) =>
        val bind = Bind()
        s"pi (${pretty_valArgs(args, bind)}): ${pretty_val(ret)}"
      case FnVal(args, ret, body, env) =>
        val bind = Bind()
        val bodyString = maybeln(pretty_exp(body))
        s"fn (${pretty_valArgs(args, bind)}): ${pretty_val(ret)} = {${bodyString}}"
      case NeuVal(neutral) =>
        val bind = Bind()
        val neutralString = maybeln(pretty_neu(neutral, bind))
        s"neutral {${neutralString}}"
      case TopVal() =>
        s"top"
      case BottomVal() =>
        s"bottom"
    }
  }

  def prettyDefine(definition: Define): String = {
    definition match {
      case DefineVal(name, value) =>
        s"define_value ${name} = ${pretty_val(value)}"

      case DefineMemberType(name, map, superName) =>
        val mapString = maybeln(pretty_exp_map(map))
        s"define_member_type ${name} <: ${superName} {${mapString}}"

      case DefineSumType(name, map, memberNames) =>
        val mapString = maybeln(pretty_exp_map(map))
        val memberNamesString = memberNames.mkString(", ")
        s"define_sum_type ${name} :> { ${memberNamesString} } {${mapString}}"

      case DefineFn(name, args, ret, body) =>
        val argsString = pretty_exp_args(args)
        val retString = pretty_exp(ret)
        val bodyString = maybeln(pretty_exp(body))
        s"define_fn ${name}(${argsString}): ${retString} = {${bodyString}}"
    }
  }

  def prettyBind(bind: Bind): String = {
    bind.map.mapValues { value => pretty_val(value) }
      .mkString("\n")
  }
}
