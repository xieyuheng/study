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

  def prettyTree(tree: Tree): String = {
    tree match {
      case Leaf(str) => '"' + str + '"'
      case Node(rule, choiceName, children) =>
        val childrenStr = maybeNewline(children.map(prettyTree).mkString("\n"))
        val argsStr = {
          if (rule.args.size == 0) {
            ""
          } else {
            val str = rule.args.map {
              case (name, rule) => s"${name} = ${rule.name}"
            }.mkString(", ")

            s" (${str})"
          }
        }
        s"${rule.name}:${choiceName}${argsStr} {${childrenStr}}"
    }
  }

  def prettyLinearTree(linearTree: LinearTree): String = {
    linearTree.parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => '"' + str + '"'
        case LinearTreePartRule(rule) => s"${rule.name}"
        case LinearTreePartBra(rule, choiceName) => s"${rule.name}:${choiceName} {"
        case LinearTreePartKet(rule, choiceName) => "}"
        case LinearTreePartPred(strPred) => strPred.toString
      }
    }
      .mkString(" ")
  }
}
