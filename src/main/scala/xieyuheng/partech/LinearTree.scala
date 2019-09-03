package xieyuheng.partech

case class LinearTree(parts: List[LinearTreePart]) {

  def length = parts.length

  def isEmpty = parts.isEmpty

  private def indexOfNextStrPred: Int = {
    parts.indexWhere { case part =>
      part.isInstanceOf[LinearTreePartPred]
    }
  }

  private def indexOfNextRule: Int = {
    parts.indexWhere { case part =>
      part.isInstanceOf[LinearTreePartRule]
    }
  }

  def complete(): Boolean = {
    indexOfNextRule == -1 && indexOfNextStrPred == -1
  }

  def tail: LinearTree = LinearTree(parts.tail)

  def head: LinearTreePart = parts.head

  def append(that: LinearTree): LinearTree = {
    LinearTree(this.parts ++ that.parts)
  }

  def consEnd(part: LinearTreePart): LinearTree = {
    LinearTree(this.parts ++ List(part))
  }

  def toStr(): String = {
    parts.map { case part =>
      part match {
        case LinearTreePartStr(str) => str
        case LinearTreePartRule(rule) => "<" ++ rule.name ++ ">"
        case LinearTreePartBra(rule, choiceName) => ""
        case LinearTreePartKet(rule, choiceName) => ""
        case LinearTreePartPred(strPred) => strPred.toString
      }
    }.mkString("")
  }

  def strLengthLowerBound: Int = {
    parts.foldLeft(0) { case (bound, part) =>
      part match {
        case LinearTreePartStr(str) => bound + str.length
        case LinearTreePartRule(rule) => bound + rule.strLengthLowerBound
        case LinearTreePartBra(rule, choiceName) => bound
        case LinearTreePartKet(rule, choiceName) => bound
        case LinearTreePartPred(strPred) => bound + strPred.length
      }
    }
  }
}

object LinearTree {
  def empty: LinearTree = {
    LinearTree(List())
  }

  def fromRule(rule: Rule): LinearTree = {
    LinearTree(List(LinearTreePartRule(rule)))
  }
}

sealed trait LinearTreePart
final case class LinearTreePartStr(str: String) extends LinearTreePart
final case class LinearTreePartRule(rule: Rule) extends LinearTreePart
final case class LinearTreePartBra(rule: Rule, choiceName: String) extends LinearTreePart
final case class LinearTreePartKet(rule: Rule, choiceName: String) extends LinearTreePart
final case class LinearTreePartPred(strPred: StrPred) extends LinearTreePart

object LinearTreePart {
  def fromRulePart(rulePart: RulePart): LinearTreePart = {
    rulePart match {
      case RulePartStr(str) => LinearTreePartStr(str)
      case RulePartRule(ruleGen) =>
        val rule = ruleGen()
        LinearTreePartRule(rule)
      case RulePartPred(strPred) =>
        LinearTreePartPred(strPred)
    }
  }
}
