package xieyuheng.partech

import xieyuheng.partech.dsl._

object RuleExample {
  def sexp = Rule(
    "sexp", Map(
      "list" -> Seq("(", sexp_list, ")"),
      "bool" -> Seq(bool)))

  def sexp_list: Rule = Rule(
    "sexp_list", Map(
      "unit" -> Seq(sexp),
      "cons" -> Seq(sexp, " ", sexp_list)))

  def bool = Rule(
    "bool", Map(
      "true" -> Seq("true"),
      "false" -> Seq("false")))
}
