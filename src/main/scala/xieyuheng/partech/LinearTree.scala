package xieyuheng.partech

case class LinearTree(parts: List[LinearTreePart]) {
  def indexOfNextVar: Int = {
    parts.indexWhere { case part =>
      part.isInstanceOf[LinearTreePartVar]
    }
  }

  def complete(): Boolean = {
    indexOfNextVar == -1
  }

  def shift(): (LinearTree, LinearTree) = {
    indexOfNextVar match {
      case -1 => (LinearTree.empty, this)
      case n => {
        val (left, right) = this.parts.splitAt(n)
        (LinearTree(left), LinearTree(right))
      }
    }
  }

  def expend(): List[LinearTree] = {
    indexOfNextVar match {
      case -1 => List()
      case n => {
        val LinearTreePartVar(rule) = parts(n)
        rule.choices.map { case (choiceName, ruleParts) =>
          val newParts = {
            List(LinearTreePartBra(rule, choiceName)) ++
            ruleParts.map(LinearTreePart.fromRulePart) ++
            List(LinearTreePartKet(rule, choiceName))
          }
          LinearTree(parts.patch(n, newParts, 1))
        } .toList
      }
    }
  }

  def append(that: LinearTree): LinearTree = {
    LinearTree(this.parts ++ that.parts)
  }

  def toStr(): String = {
    parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => str
        case LinearTreePartVar(rule) => "<" ++ rule.name ++ ">"
        case LinearTreePartBra(rule, choiceName) => ""
        case LinearTreePartKet(rule, choiceName) => ""
      }
    }
    .mkString("")
  }

  def toStrWithoutVar(): String = {
    parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => str
        case LinearTreePartVar(rule) => ""
        case LinearTreePartBra(rule, choiceName) => ""
        case LinearTreePartKet(rule, choiceName) => ""
      }
    }
    .mkString("")
  }
}

object LinearTree {
  def empty: LinearTree = {
    LinearTree(List())
  }

  def fromRule(rule: Rule): LinearTree = {
    LinearTree(List(LinearTreePartVar(rule)))
  }
}

sealed trait LinearTreePart
final case class LinearTreePartStr(str: String) extends LinearTreePart
final case class LinearTreePartVar(rule: Rule) extends LinearTreePart
final case class LinearTreePartBra(rule: Rule, choiceName: String) extends LinearTreePart
final case class LinearTreePartKet(rule: Rule, choiceName: String) extends LinearTreePart

object LinearTreePart {
  def fromRulePart(rulePart: RulePart): LinearTreePart = {
    rulePart match {
      case RulePartStr(str) => LinearTreePartStr(str)
      case RulePartRule(ruleGen) => {
        val rule = ruleGen()
        LinearTreePartVar(rule)
      }
    }
  }
}
