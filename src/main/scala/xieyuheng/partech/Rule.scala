package xieyuheng.partech

case class Rule(
  name: String,
  choices: Map[String, Seq[RulePart]],
  args: Map[String, Rule] = Map())

sealed trait RulePart

final case class RulePartStr(str: String) extends RulePart {
  override def toString = {
    '"' + str + '"'
  }
}

final case class RulePartRule(ruleGen: () => Rule) extends RulePart {
  override def toString = {
    ruleGen().name
  }
}
