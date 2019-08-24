package xieyuheng.partech

case class Rule(name: String, choices: Map[String, Seq[Part]])

sealed trait Part

final case class StrPart(str: String) extends Part {
  override def toString = {
    '"' + str + '"'
  }
}

final case class RulePart(rule: () => Rule) extends Part

object RuleTest extends App {

  implicit def StrPartFromString(str: String): StrPart = {
    StrPart(str)
  }

  implicit def RulePartFromRule(rule: => Rule): RulePart = {
    RulePart(() => rule)
  }

  // sexp:list = "(" sexp-list ")";
  // sexp:bool = bool;

  // sexp-list:unit = sexp;
  // sexp-list:cons = sexp " " sexp-list;

  // bool:true = "true";
  // bool:false = "false";

  def sexp: Rule = Rule("sexp", Map(
    "list" -> Seq("(", sexp_list, ")"),
    "bool" -> Seq(bool)))

  def sexp_list: Rule = Rule("sexp_list", Map(
    "unit" -> Seq(sexp),
    "cons" -> Seq(sexp, " ", sexp_list)))

  def bool: Rule = Rule("bool", Map(
    "true" -> Seq("true"),
    "false" -> Seq("false")))

  sexp.choices.foreach(println)
}
