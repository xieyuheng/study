package xieyuheng.cicada

import scala.collection.immutable.ListMap

object pretty {

  val IndentUnit: String = "  "

  def getIndent(level: Int): String = {
    assert(level >= 0)
    IndentUnit * level
  }

  def addIndentToBlock(block: String, level: Int): String = {
    block
      .split("\n")
      .map(getIndent(level) ++ _)
      .mkString("\n")
  }

  def maybeNewline(string: String): String = {
    if (string.trim.isEmpty) {
      ""
    } else {
      "\n" ++ addIndentToBlock(string, 1) ++ "\n"
    }
  }

  def prettyExpMapWithDelimiter(
    map: MultiMap[String, Exp],
    delimiter: String,
  ): String = {
    map.entries
      .map { case (name, exp) => s"${name}: ${prettyExp(exp)}" }
      .mkString(delimiter)
  }

  def prettyExpMap(map: MultiMap[String, Exp]): String = {
    prettyExpMapWithDelimiter(map, "\n")
  }

  def prettyExpArgs(map: MultiMap[String, Exp]): String = {
    prettyExpMapWithDelimiter(map, ", ")
  }

  def prettyExp(exp: Exp): String = {
    exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case The(t) =>
        s"the(${prettyExp(t)})"
      case Choice(target, map) =>
        val mapString = maybeNewline(prettyExpMap(map))
        s"${prettyExp(target)} case {${mapString}}"
      case Dot(target, fieldName) =>
        s"${prettyExp(target)}.${fieldName}"
      case Pi(args, ret) =>
        s"pi (${prettyExpArgs(args)}): ${prettyExp(ret)}"
      case Fn(args, ret, body) =>
        val bodyString = maybeNewline(prettyExp(body))
        s"fn (${prettyExpArgs(args)}): ${prettyExp(ret)} = {${bodyString}}"
      case Ap(target, args) =>
        s"${prettyExp(target)}(${prettyExpArgs(args)})"
    }
  }

  def prettyValueMapWithDelimiter(
    map: ListMap[String, Value],
    bind: Bind,
    delimiter: String,
  ): String = {
    walk.deepOnMap(map, bind)
      .map { case (name, value) => s"${name}: ${prettyValue(value)}" }
      .mkString(delimiter)
  }

  def prettyValueMap(map: ListMap[String, Value], bind: Bind): String = {
    prettyValueMapWithDelimiter(map, bind, "\n")
  }

  def prettyValueArgs(map: ListMap[String, Value], bind: Bind): String = {
    prettyValueMapWithDelimiter(map, bind, ", ")
  }

  def prettyNeutral(neutral: Neutral, bind: Bind): String = {
    neutral match {
      case VarNeutral(name) =>
        name
      case ChoiceNeutral(target, map) =>
        val mapString = maybeNewline(prettyValueMap(map, bind))
        s"${prettyNeutral(target, bind)} case {${mapString}}"
      case DotNeutral(target, fieldName) =>
        s"${prettyNeutral(target, bind)}.${fieldName}"
      case ApNeutral(target, args) =>
        s"${prettyNeutral(target, bind)}(${prettyValueArgs(args, bind)})"
    }
  }

  def prettyValue(value: Value): String = {
    value match {
      case TypeOfType(id) =>
        s"type(${id})"
      case ValueOfType(id, t) =>
        s"the(${id}, ${prettyValue(t)})"
      case SumTypeValue(name, map, memberNames, bind) =>
        val mapString = maybeNewline(prettyValueMap(map, bind))
        s"${name} {${mapString}}"
      case MemberTypeValue(name, map, superName, bind) =>
        val mapString = maybeNewline(prettyValueMap(map, bind))
        s"${name} {${mapString}}"
      case PiValue(args, ret) =>
        val bind = Bind()
        s"pi (${prettyValueArgs(args, bind)}): ${prettyValue(ret)}"
      case FnValue(args, ret, body, env) =>
        val bind = Bind()
        val bodyString = maybeNewline(prettyExp(body))
        s"fn (${prettyValueArgs(args, bind)}): ${prettyValue(ret)} = {${bodyString}}"
      case NeutralValue(neutral) =>
        val bind = Bind()
        val neutralString = maybeNewline(prettyNeutral(neutral, bind))
        s"neutral {${neutralString}}"
      case TopValue() =>
        s"top"
      case BottomValue() =>
        s"bottom"
    }
  }

  def prettyDefine(definition: Define): String = {
    definition match {
      case DefineValue(name, value) =>
        s"define_value ${name} = ${prettyValue(value)}"

      case DefineMemberType(name, map, superName) =>
        val mapString = maybeNewline(prettyExpMap(map))
        s"define_member_type ${name} <: ${superName} {${mapString}}"

      case DefineSumType(name, map, memberNames) =>
        val mapString = maybeNewline(prettyExpMap(map))
        val memberNamesString = memberNames.mkString(", ")
        s"define_sum_type ${name} :> { ${memberNamesString} } {${mapString}}"

      case DefineFn(name, args, ret, body) =>
        val argsString = prettyExpArgs(args)
        val retString = prettyExp(ret)
        val bodyString = maybeNewline(prettyExp(body))
        s"define_fn ${name}(${argsString}): ${retString} = {${bodyString}}"
    }
  }

  def prettyBind(bind: Bind): String = {
    bind.map.mapValues { value => prettyValue(value) }
      .mkString("\n")
  }
}
