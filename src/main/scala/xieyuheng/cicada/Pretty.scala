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
      string ++ "\n"
    }
  }

  def ExpMapWithDelimiter(
    map: MultiMap[String, Exp],
    level: Int,
    delimiter: String,
  ): String = {
    val block = map.entries
      .map { case (name, exp) => s"${name}: ${Pretty.Exp(exp, 0)}" }
      .mkString(delimiter)

    addIndentToBlock(block, level)
  }

  def ExpMap(map: MultiMap[String, Exp], level: Int): String = {
    Pretty.ExpMapWithDelimiter(map, level, "\n")
  }

  def ExpArgs(map: MultiMap[String, Exp], level: Int): String = {
    Pretty.ExpMapWithDelimiter(map, level, ", ")
  }

  def Exp(exp: Exp, level: Int): String = {
    val block = exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case The(t) =>
        s"the(${Pretty.Exp(t, 0)})"
      case Case(target, map) =>
        val mapString = maybeNewline(Pretty.ExpMap(map, 1))
        s"${Pretty.Exp(target, 0)} case {\n${mapString}}"
      case Field(target, fieldName) =>
        s"${Pretty.Exp(target, 0)}.${fieldName}"
      case Pi(args, ret) =>
        s"pi (${Pretty.ExpArgs(args, 0)}): ${Pretty.Exp(ret, 0)}"
      case Fn(args, ret, body) =>
        val bodyString = maybeNewline(Pretty.Exp(body, 1))
        s"fn (${Pretty.ExpArgs(args, 0)}): ${Pretty.Exp(ret, 0)} = {\n${bodyString}}"
      case Ap(target, args) =>
        s"${Pretty.Exp(target, 0)}(${Pretty.ExpArgs(args, 0)})"
    }

    addIndentToBlock(block, level)
  }

  def ValueMapWithDelimiter(
    map: MultiMap[String, Value],
    bind: Bind,
    level: Int,
    delimiter: String,
  ): String = {
    val block = walk.deepOnMap(map, bind)
      .entries
      .map { case (name, value) =>
        s"${name}: ${Pretty.Value(value, 0)}" }
      .mkString(delimiter)

    addIndentToBlock(block, level)
  }

  def ValueMap(map: MultiMap[String, Value], bind: Bind, level: Int): String = {
    Pretty.ValueMapWithDelimiter(map, bind, level, "\n")
  }

  def ValueArgs(map: MultiMap[String, Value], bind: Bind, level: Int): String = {
    Pretty.ValueMapWithDelimiter(map, bind, level, ", ")
  }

  def Neutral(neutral: Neutral, bind: Bind, level: Int): String = {
    val block = neutral match {
      case VarNeutral(name) =>
        name
      case CaseNeutral(target, map) =>
        val mapString = maybeNewline(Pretty.ValueMap(map, bind, 1))
        s"${Pretty.Neutral(target, bind, 0)} case {\n${mapString}}"
      case FieldNeutral(target, fieldName) =>
        s"${Pretty.Neutral(target, bind, 0)}.${fieldName}"
      case ApNeutral(target, args) =>
        s"${Pretty.Neutral(target, bind, 0)}(${Pretty.ValueArgs(args, bind, 0)})"
    }

    addIndentToBlock(block, level)
  }

  def Value(value: Value, level: Int): String = {
    val block = value match {
      case TypeOfType(id) =>
        s"type(${id})"
      case ValueOfType(id, t) =>
        s"the(${id}, ${Pretty.Value(t, 0)})"
      case SumTypeValue(name, map, memberNames, bind) =>
        val memberNamesString = maybeNewline(memberNames.mkString(", "))
        val mapString = maybeNewline(Pretty.ValueMap(map, bind, 1))
        s"${name} {\n${mapString}}"
      case MemberTypeValue(name, map, superName, bind) =>
        val mapString = maybeNewline(Pretty.ValueMap(map, bind, 1))
        s"${name} {\n${mapString}}"
      case PiValue(args, ret) =>
        val bind = Bind()
        s"pi (${Pretty.ValueArgs(args, bind, 0)}): ${Pretty.Value(ret, 0)}"
      case FnValue(args, ret, body, env) =>
        val bind = Bind()
        val bodyString = maybeNewline(Pretty.Exp(body, 1))
        s"fn (${Pretty.ValueArgs(args, bind, 0)}): ${Pretty.Value(ret, 0)} = {\n${bodyString}}"
      case NeutralValue(neutral) =>
        val bind = Bind()
        val neutralString = maybeNewline(Pretty.Neutral(neutral, bind, 1))
        s"neutral {\n${neutralString}}"
    }

    addIndentToBlock(block, level)
  }
}
