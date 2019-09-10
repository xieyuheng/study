package xieyuheng.partech

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

  def getArgsStr(rule: Rule): String = {
    if (rule.args.size == 0) {
      ""
    } else {
      val str = rule.args.map {
        case (name, rule) => s"${name} = ${rule.name}"
      }.mkString(", ")

      s"(${str})"
    }
  }

  def prettyTree(tree: Tree): String = {
    tree match {
      case Leaf(str) => '"' + str + '"'
      case Node(rule, choiceName, children) =>
        val childrenStr = maybeNewline(children.map(prettyTree).mkString("\n"))
        s"${rule.name}::${choiceName}${getArgsStr(rule)} {${childrenStr}}"
    }
  }

  def prettyLinearTree(parts: List[LinearTreePart]): String = {
    parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => '"' + str + '"'
        case LinearTreePartRule(rule) => s"${rule.name}"
        case LinearTreePartBra(rule, choiceName) =>
          s"<${rule.name}::${choiceName}${getArgsStr(rule)}>"
        case LinearTreePartKet(rule, choiceName) =>
          s"</${rule.name}::${choiceName}${getArgsStr(rule)}>"
        case LinearTreePartPred(wordPred) => s"[${wordPred.name}]"
      }
    }.mkString(" ")
  }
}
