package xieyuheng.partech

object pretty {

  val INDENT_UNIT: String = "  "

  def get_indent(level: Int): String = {
    assert(level >= 0)
    INDENT_UNIT * level
  }

  def add_indent_to_block(block: String, level: Int): String = {
    block
      .split("\n")
      .map(get_indent(level) ++ _)
      .mkString("\n")
  }

  def maybeln(string: String): String = {
    if (string.trim.isEmpty) {
      ""
    } else {
      "\n" ++ add_indent_to_block(string, 1) ++ "\n"
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
        val childrenStr = maybeln(children.map(prettyTree).mkString("\n"))
        s"${rule.name}::${choiceName}${getArgsStr(rule)} {${childrenStr}}"
    }
  }
}
