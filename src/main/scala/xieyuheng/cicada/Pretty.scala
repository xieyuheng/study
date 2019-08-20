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

  def fromExpMapWithDelimiter(
    map: MultiMap[String, Exp],
    level: Int,
    delimiter: String,
  ): String = {
    val block = map.entries
      .map { case (name, exp) =>
        s"${name}: ${fromExp(exp, level)}" }
      .mkString(delimiter)

    addIndentToBlock(block, level)
  }

  def fromExpMap(map: MultiMap[String, Exp], level: Int): String = {
    fromExpMapWithDelimiter(map, level, "\n")
  }

  def fromExpArgs(args: MultiMap[String, Exp], level: Int): String = {
    fromExpMapWithDelimiter(args, level, ", ")
  }

  def fromExp(exp: Exp, level: Int): String = {
    val block = exp match {
      case Var(name) =>
        name
      case Type() =>
        "Type"
      case Case(target, map) =>
        s"${fromExp(target, 0)} case {\n${fromExpMap(map, 1)}\n}"
      case Field(target, fieldName) =>
        s"${fromExp(target, 0)}.${fieldName}"
      case Pi(args, ret) =>
        s"(${fromExpArgs(args, 0)}): ${fromExp(ret, 0)}"
      case Fn(args, ret, body) =>
        s"(${fromExpArgs(args, 0)}): ${fromExp(ret, 0)} = {\n${fromExp(body, 1)}\n}"
      case Ap(target, args) =>
        s"${fromExp(target, 0)}(${fromExpArgs(args, 0)})"
    }

    addIndentToBlock(block, level)
  }

  def fromValue(value: Value, level: Int): String = {
    ???
  }
}
