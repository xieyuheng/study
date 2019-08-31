package xieyuheng.partech

case class LinearParseTree(parts: List[LinearParsePart]) {
  def indexOfNextVar: Int = {
    parts.indexWhere { case part =>
      part.isInstanceOf[LinearParseVar]
    }
  }

  def complete(): Boolean = {
    indexOfNextVar == -1
  }

  def shift(): (LinearParseTree, LinearParseTree) = {
    indexOfNextVar match {
      case -1 => (LinearParseTree.empty, this)
      case n => {
        val (left, right) = this.parts.splitAt(n)
        (LinearParseTree(left), LinearParseTree(right))
      }
    }
  }

  def expend(): List[LinearParseTree] = {
    indexOfNextVar match {
      case -1 => List()
      case n => {
        val LinearParseVar(rule) = parts(n)
        rule.choices.map { case (choiceName, ruleParts) =>
          val newParts = {
            List(LinearParseBra(rule, choiceName)) ++
            ruleParts.map(LinearParsePart.fromRulePart) ++
            List(LinearParseKet(rule, choiceName))
          }
          LinearParseTree(parts.patch(n, newParts, 1))
        } .toList
      }
    }
  }

  def append(that: LinearParseTree): LinearParseTree = {
    LinearParseTree(this.parts ++ that.parts)
  }

  def toPretty(): String = {
    parts.map { case part =>
      part match {
        case LinearParseStr(str) => '"' + str + '"'
        case LinearParseVar(rule) => s"${rule.name}"
        case LinearParseBra(rule, choiceName) => s"${rule.name}:${choiceName} {"
        case LinearParseKet(rule, choiceName) => "}"
      }
    }
    .mkString(" ")
  }

  def toStr(): String = {
    parts.map { case part =>
      part match {
        case LinearParseStr(str) => str
        case LinearParseVar(rule) => "<" ++ rule.name ++ ">"
        case LinearParseBra(rule, choiceName) => ""
        case LinearParseKet(rule, choiceName) => ""
      }
    }
    .mkString("")
  }

  def toStrWithoutVar(): String = {
    parts.map { case part =>
      part match {
        case LinearParseStr(str) => str
        case LinearParseVar(rule) => ""
        case LinearParseBra(rule, choiceName) => ""
        case LinearParseKet(rule, choiceName) => ""
      }
    }
    .mkString("")
  }
}

object LinearParseTree {
  def empty: LinearParseTree = {
    LinearParseTree(List())
  }

  def fromRule(rule: Rule): LinearParseTree = {
    LinearParseTree(List(LinearParseVar(rule)))
  }
}

sealed trait LinearParsePart
final case class LinearParseStr(str: String) extends LinearParsePart
final case class LinearParseVar(rule: Rule) extends LinearParsePart
final case class LinearParseBra(rule: Rule, choiceName: String) extends LinearParsePart
final case class LinearParseKet(rule: Rule, choiceName: String) extends LinearParsePart

object LinearParsePart {
  def fromRulePart(rulePart: RulePart): LinearParsePart = {
    rulePart match {
      case RulePartStr(str) => LinearParseStr(str)
      case RulePartRule(ruleGen) => {
        val rule = ruleGen()
        LinearParseVar(rule)
      }
    }
  }
}
