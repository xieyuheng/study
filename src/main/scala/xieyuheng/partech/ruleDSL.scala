package xieyuheng.partech

object ruleDSL {

  implicit def RulePartStrFromString(str: String): RulePartStr = {
    RulePartStr(str)
  }

  implicit def RulePartRuleFromRule(rule: => Rule): RulePartRule = {
    RulePartRule(() => rule)
  }

  implicit def RulePartPredFromWordPred(wordPred: WordPred): RulePartPred = {
    RulePartPred(wordPred)
  }

}
