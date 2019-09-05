package xieyuheng.partech

case class Rule(
  name: String,
  choices: Map[String, Seq[RulePart]],
  args: Map[String, Rule] = Map(),
) {
  assert(choices.size > 0)

  // just to get lower bound
  // - we can not use `==`, because we can not compare lambda (ruleGen)
  //   but it is ok to mis-comparing some rules to be the same
  //   the lower bound will not be the greatest lower bound
  private def similar(that: Rule): Boolean = {
    this.name == that.name &&
    this.choices.keys.toSet == that.choices.keys.toSet &&
    this.args.keys.toSet == that.args.keys.toSet
  }

  lazy val lowerBound: Int = Rule.lowerBound(this, List(this))
}

object Rule {
  def seq(name: String, parts: Seq[RulePart]): Rule = {
    Rule(name, Map(name -> parts))
  }

  private def lowerBound(rule: Rule, occured: List[Rule]): Int = {
    rule.choices.map { case (_name, parts) =>
      parts.foldLeft(0) { case (bound, part) =>
        part match {
          case RulePartStr(str) => bound + 1
          case RulePartRule(ruleGen) =>
            val r = ruleGen()
            if (occured.exists(r.similar)) {
              bound
            } else {
              bound + Rule.lowerBound(r, r :: occured)
            }
          case RulePartPred(pred) => bound + 1
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

final case class RulePartPred(pred: String => Boolean) extends RulePart
