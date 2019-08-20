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

  def fromExpMapWithDelimiter(
    map: MultiMap[String, Exp],
    level: Int,
    delimiter: String,
  ): String = {
    val block = map.entries
      .map { case (name, exp) =>
        s"${name}: ${fromExp(exp, 0)}" }
      .mkString(delimiter)

    addIndentToBlock(block, level)
  }

  def fromExpMap(map: MultiMap[String, Exp], level: Int): String = {
    fromExpMapWithDelimiter(map, level, "\n")
  }

  def fromExpArgs(map: MultiMap[String, Exp], level: Int): String = {
    fromExpMapWithDelimiter(map, level, ", ")
  }

  def fromExp(exp: Exp, level: Int): String = {
    val block = exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case Case(target, map) =>
        val mapString = maybeNewline(fromExpMap(map, 1))
        s"${fromExp(target, 0)} case {\n${mapString}}"
      case Field(target, fieldName) =>
        s"${fromExp(target, 0)}.${fieldName}"
      case Pi(args, ret) =>
        s"(${fromExpArgs(args, 0)}): ${fromExp(ret, 0)}"
      case Fn(args, ret, body) =>
        val bodyString = maybeNewline(fromExp(body, 1))
        s"(${fromExpArgs(args, 0)}): ${fromExp(ret, 0)} = {\n${bodyString}}"
      case Ap(target, args) =>
        s"${fromExp(target, 0)}(${fromExpArgs(args, 0)})"
    }

    addIndentToBlock(block, level)
  }

  def fromValueMapWithDelimiter(
    map: MultiMap[String, Value],
    level: Int,
    delimiter: String,
  ): String = {
    val block = map.entries
      .map { case (name, value) =>
        s"${name}: ${fromValue(value, 0)}" }
      .mkString(delimiter)

    addIndentToBlock(block, level)
  }

  def fromValueMap(map: MultiMap[String, Value], level: Int): String = {
    fromValueMapWithDelimiter(map, level, "\n")
  }

  def fromValueArgs(map: MultiMap[String, Value], level: Int): String = {
    fromValueMapWithDelimiter(map, level, ", ")
  }

  def fromNeutral(neutral: Neutral, level: Int): String = {
    val block = neutral match {
      case VarNeutral(name) =>
        name
      case CaseNeutral(target, map) =>
        val mapString = maybeNewline(fromValueMap(map, 1))
        s"${fromNeutral(target, 0)} case {\n${mapString}}"
      case FieldNeutral(target, fieldName) =>
        s"${fromNeutral(target, 0)}.${fieldName}"
      case ApNeutral(target, args) =>
        s"${fromNeutral(target, 0)}(${fromValueArgs(args, 0)})"
    }

    addIndentToBlock(block, level)
  }

  def fromValue(value: Value, level: Int): String = {
    val block = value match {
      case LogicVar(id) =>
        s"#${id}"
      case UnionValue(id, name, map, subNames, bind) =>
        val subNamesString = maybeNewline(subNames.mkString(", "))
        val mapString = maybeNewline(fromValueMap(map, 1))
        s"union ${name} {\n${mapString}} unions {\n  ${subNamesString}}"
      case RecordValue(name, map, bind) =>
        val mapString = maybeNewline(fromValueMap(map, 1))
        s"record ${name} {\n${mapString}}"
      case PiValue(id, args, ret) =>
        s"(${fromValueArgs(args, 0)}): ${fromValue(ret, 0)}"
      case FnValue(args, ret, body, env) =>
        val bodyString = maybeNewline(fromExp(body, 1))
        s"(${fromValueArgs(args, 0)}): ${fromValue(ret, 0)} = {\n${bodyString}}"
      case NeutralValue(neutral) =>
        val neutralString = maybeNewline(fromNeutral(neutral, 1))
        s"Neutral {\n${neutralString}}"
    }

    addIndentToBlock(block, level)
  }
}
