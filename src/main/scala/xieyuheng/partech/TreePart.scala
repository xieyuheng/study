package xieyuheng.partech

sealed trait LinearTreePart {
  def lowerBound: Int = {
    this match {
      case LinearTreePartStr(str) => 1
      case LinearTreePartRule(rule) => rule.lowerBound
      case LinearTreePartBra(rule, choiceName) => 0
      case LinearTreePartKet(rule, choiceName) => 0
      case LinearTreePartPred(pred) => 1
    }
  }
}

final case class LinearTreePartStr(str: String) extends LinearTreePart
final case class LinearTreePartRule(rule: Rule) extends LinearTreePart
final case class LinearTreePartBra(rule: Rule, choiceName: String) extends LinearTreePart
final case class LinearTreePartKet(rule: Rule, choiceName: String) extends LinearTreePart
final case class LinearTreePartPred(pred: String => Boolean) extends LinearTreePart

object LinearTreePart {
  def fromRulePart(rulePart: RulePart): LinearTreePart = {
    rulePart match {
      case RulePartStr(str) => LinearTreePartStr(str)
      case RulePartRule(ruleGen) => LinearTreePartRule(ruleGen())
      case RulePartPred(pred) => LinearTreePartPred(pred)
    }
  }
}
