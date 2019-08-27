package xieyuheng.partech

object dsl {
  implicit def StrPartFromString(str: String): RulePartStr = {
    RulePartStr(str)
  }

  implicit def RulePartFromRule(rule: => Rule): RulePartRule = {
    RulePartRule(() => rule)
  }
}
