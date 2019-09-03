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
        s"${rule.name}:${choiceName} {${childrenStr}}"
    }
  }

  def prettyLinearTree(linearTree: LinearTree): String = {
    linearTree.parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => '"' + str + '"'
        case LinearTreePartVar(rule) => s"${rule.name}"
        case LinearTreePartBra(rule, choiceName) => s"${rule.name}:${choiceName} {"
        case LinearTreePartKet(rule, choiceName) => "}"
      }
    }
      .mkString(" ")
  }
}
