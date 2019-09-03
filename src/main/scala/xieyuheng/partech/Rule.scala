package xieyuheng.partech

case class Rule(
  name: String,
  choices: Map[String, Seq[RulePart]],
  args: Map[String, Rule] = Map(),
) {
  assert(choices.size > 0)
}

sealed trait RulePart

final case class RulePartStr(str: String) extends RulePart {
  assert(str.length > 0)

  override def toString = {
    '"' + str + '"'
  }
}

final case class RulePartRule(ruleGen: () => Rule) extends RulePart {
  override def toString = {
    ruleGen().name
  }
}

final case class RulePartPred(strPred: StrPred) extends RulePart {
  override def toString = {
    strPred.toString
  }
}
