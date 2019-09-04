package xieyuheng.partech.lexerless

object ruleDSL {

  implicit def RulePartStrFromString(str: String): RulePartStr = {
    RulePartStr(str)
  }

  implicit def RulePartRuleFromRule(rule: => Rule): RulePartRule = {
    RulePartRule(() => rule)
  }

  implicit def RulePartPredFromStrPred(strPred: StrPred): RulePartPred = {
    RulePartPred(strPred)
  }

  implicit def RuleFromStrPred(strPred: StrPred): Rule = {
    Rule(
      "$" ++ strPred.name, Map(
        strPred.name -> Seq(RulePartPred(strPred))))
  }
}
