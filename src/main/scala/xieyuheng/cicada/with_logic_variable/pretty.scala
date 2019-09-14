package xieyuheng.cicada.with_logic_variable

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

  def prettyValMapWithDelimiter(
    map: ListMap[String, Val],
    bind: Bind,
    delimiter: String,
  ): String = {
    walk.deepOnMap(map, bind)
      .map { case (name, value) => s"${name}: ${prettyVal(value)}" }
      .mkString(delimiter)
  }

  def prettyValMap(map: ListMap[String, Val], bind: Bind): String = {
    prettyValMapWithDelimiter(map, bind, "\n")
  }

  def prettyValArgs(map: ListMap[String, Val], bind: Bind): String = {
    prettyValMapWithDelimiter(map, bind, ", ")
  }

  def prettyNeu(neutral: Neu, bind: Bind): String = {
    neutral match {
      case VarNeu(name) =>
        name
      case ChoiceNeu(target, map) =>
        val mapString = maybeNewline(prettyValMap(map, bind))
        s"${prettyNeu(target, bind)} case {${mapString}}"
      case DotNeu(target, fieldName) =>
        s"${prettyNeu(target, bind)}.${fieldName}"
      case ApNeu(target, args) =>
        s"${prettyNeu(target, bind)}(${prettyValArgs(args, bind)})"
    }
  }

  def prettyVal(value: Val): String = {
    value match {
      case TypeOfType(id) =>
        s"type(${id})"
      case ValOfType(id, t) =>
        s"the(${id}, ${prettyVal(t)})"
      case SumTypeVal(name, map, memberNames, bind) =>
        val mapString = maybeNewline(prettyValMap(map, bind))
        s"${name} {${mapString}}"
      case MemberTypeVal(name, map, superName, bind) =>
        val mapString = maybeNewline(prettyValMap(map, bind))
        s"${name} {${mapString}}"
      case PiVal(args, ret) =>
        val bind = Bind()
        s"pi (${prettyValArgs(args, bind)}): ${prettyVal(ret)}"
      case FnVal(args, ret, body, env) =>
        val bind = Bind()
        val bodyString = maybeNewline(prettyExp(body))
        s"fn (${prettyValArgs(args, bind)}): ${prettyVal(ret)} = {${bodyString}}"
      case NeuVal(neutral) =>
        val bind = Bind()
        val neutralString = maybeNewline(prettyNeu(neutral, bind))
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
        s"define_value ${name} = ${prettyVal(value)}"

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
    bind.map.mapValues { value => prettyVal(value) }
      .mkString("\n")
  }
}
