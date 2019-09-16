package xieyuheng.partech

import xieyuheng.util.pretty._

object pretty {

  def get_args_str(rule: Rule): String = {
    if (rule.args.size == 0) {
      ""
    } else {
      val str = rule.args.map {
        case (name, rule) => s"${name} = ${rule.name}"
      }.mkString(", ")

      s"(${str})"
    }
  }

  def pretty_tree(tree: Tree): String = {
    tree match {
      case Leaf(str) => '"' + str + '"'
      case Node(rule, choiceName, children) =>
        val childrenStr = maybeln(children.map(pretty_tree).mkString("\n"))
        s"${rule.name}::${choiceName}${get_args_str(rule)} {${childrenStr}}"
    }
  }
}
