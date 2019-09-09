package xieyuheng.partech.lexerless

case class Rule(
  name: String,
  choices: Map[String, List[RulePart]],
  args: Map[String, Rule] = Map(),
) {
  assert(choices.size > 0)

  // just for defining Rule.getStrLengthLowerBound and strLengthLowerBound
  // - for we can not compare lambda (ruleGen)
  //   but it is ok to mis-comparing some rules to be the same
  private def similar(that: Rule): Boolean = {
    this.name == that.name &&
    this.choices.keys.toSet == that.choices.keys.toSet &&
    this.args.keys.toSet == that.args.keys.toSet
  }

  lazy val strLengthLowerBound: Int = Rule.getStrLengthLowerBound(this, List(this))
}

object Rule {
  private def getStrLengthLowerBound(rule: Rule, occured: List[Rule]): Int = {
    rule.choices.map { case (_name, parts) =>
      parts.foldLeft(0) { case (bound, part) =>
        part match {
          case RulePartStr(str) => bound + str.length
          case RulePartRule(ruleGen) =>
            val r = ruleGen()
            if (occured.exists(r.similar)) {
              bound
            } else {
              bound + getStrLengthLowerBound(r, r :: occured)
            }
          case RulePartPred(strPred) => bound + strPred.length
        }
      }
    }.min
  }
}

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

final case class RulePartPred(strPred: StrPred) extends RulePart {
  override def toString = {
    strPred.toString
  }
}
