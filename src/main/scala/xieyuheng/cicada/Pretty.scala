package xieyuheng.cicada

object Pretty {

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

  def ExpMapWithDelimiter(
    map: MultiMap[String, Exp],
    delimiter: String,
  ): String = {
    map.entries
      .map { case (name, exp) => s"${name}: ${Pretty.Exp(exp)}" }
      .mkString(delimiter)
  }

  def ExpMap(map: MultiMap[String, Exp]): String = {
    Pretty.ExpMapWithDelimiter(map, "\n")
  }

  def ExpArgs(map: MultiMap[String, Exp]): String = {
    Pretty.ExpMapWithDelimiter(map, ", ")
  }

  def Exp(exp: Exp): String = {
    exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case The(t) =>
        s"the(${Pretty.Exp(t)})"
      case Case(target, map) =>
        val mapString = maybeNewline(Pretty.ExpMap(map))
        s"${Pretty.Exp(target)} case {${mapString}}"
      case Field(target, fieldName) =>
        s"${Pretty.Exp(target)}.${fieldName}"
      case Pi(args, ret) =>
        s"pi (${Pretty.ExpArgs(args)}): ${Pretty.Exp(ret)}"
      case Fn(args, ret, body) =>
        val bodyString = maybeNewline(Pretty.Exp(body))
        s"fn (${Pretty.ExpArgs(args)}): ${Pretty.Exp(ret)} = {${bodyString}}"
      case Ap(target, args) =>
        s"${Pretty.Exp(target)}(${Pretty.ExpArgs(args)})"
    }
  }

  def ValueMapWithDelimiter(
    map: MultiMap[String, Value],
    bind: Bind,
    delimiter: String,
  ): String = {
    walk.deepOnMap(map, bind)
      .entries
      .map { case (name, value) =>
        s"${name}: ${Pretty.Value(value)}" }
      .mkString(delimiter)
  }

  def ValueMap(map: MultiMap[String, Value], bind: Bind): String = {
    Pretty.ValueMapWithDelimiter(map, bind, "\n")
  }

  def ValueArgs(map: MultiMap[String, Value], bind: Bind): String = {
    Pretty.ValueMapWithDelimiter(map, bind, ", ")
  }

  def Neutral(neutral: Neutral, bind: Bind): String = {
    neutral match {
      case VarNeutral(name) =>
        name
      case CaseNeutral(target, map) =>
        val mapString = maybeNewline(Pretty.ValueMap(map, bind))
        s"${Pretty.Neutral(target, bind)} case {${mapString}}"
      case FieldNeutral(target, fieldName) =>
        s"${Pretty.Neutral(target, bind)}.${fieldName}"
      case ApNeutral(target, args) =>
        s"${Pretty.Neutral(target, bind)}(${Pretty.ValueArgs(args, bind)})"
    }
  }

  def Value(value: Value): String = {
    value match {
      case TypeOfType(id) =>
        s"type(${id})"
      case ValueOfType(id, t) =>
        s"the(${id}, ${Pretty.Value(t)})"
      case SumTypeValue(name, map, memberNames, bind) =>
        val mapString = maybeNewline(Pretty.ValueMap(map, bind))
        s"${name} {${mapString}}"
      case MemberTypeValue(name, map, superName, bind) =>
        val mapString = maybeNewline(Pretty.ValueMap(map, bind))
        s"${name} {${mapString}}"
      case PiValue(args, ret) =>
        val bind = Bind()
        s"pi (${Pretty.ValueArgs(args, bind)}): ${Pretty.Value(ret)}"
      case FnValue(args, ret, body, env) =>
        val bind = Bind()
        val bodyString = maybeNewline(Pretty.Exp(body))
        s"fn (${Pretty.ValueArgs(args, bind)}): ${Pretty.Value(ret)} = {${bodyString}}"
      case NeutralValue(neutral) =>
        val bind = Bind()
        val neutralString = maybeNewline(Pretty.Neutral(neutral, bind))
        s"neutral {${neutralString}}"
    }
  }

  def Def(definition: Def): String = {
    definition match {
      case DefineValue(name, value) =>
        s"define_value ${name} = ${Pretty.Value(value)}"

      case DefineMemberType(name, map, superName) =>
        val mapString = maybeNewline(Pretty.ExpMap(map))
        s"define_member_type ${name} <: ${superName} {${mapString}}"

      case DefineSumType(name, map, memberNames) =>
        val mapString = maybeNewline(Pretty.ExpMap(map))
        val memberNamesString = memberNames.mkString(", ")
        s"define_sum_type ${name} :> { ${memberNamesString} } {${mapString}}"

      case DefineFn(name, args, ret, body) =>
        val argsString = Pretty.ExpArgs(args)
        val retString = Pretty.Exp(ret)
        val bodyString = maybeNewline(Pretty.Exp(body))
        s"define_fn ${name}(${argsString}): ${retString} = {${bodyString}}"
    }
  }
}
