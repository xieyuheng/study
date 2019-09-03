package xieyuheng.partech

object ruleDSL {

  implicit def RulePartStrFromString(str: String): RulePartStr = {
    RulePartStr(str)
  }

  implicit def RulePartRuleFromRule(rule: => Rule): RulePartRule = {
    RulePartRule(() => rule)
  }

  implicit def RulePartPredFromRule(strPred: StrPred): RulePartPred = {
    RulePartPred(strPred)
  }

}
