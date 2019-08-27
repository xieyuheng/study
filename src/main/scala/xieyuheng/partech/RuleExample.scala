package xieyuheng.partech

import xieyuheng.partech.dsl._

object RuleExample {
  def bool_sexp = Rule(
    "bool_sexp", Map(
      "list" -> Seq("(", bool_sexp_list, ")"),
      "bool" -> Seq(bool)))

  def bool_sexp_list: Rule = Rule(
    "bool_sexp_list", Map(
      "unit" -> Seq(bool_sexp),
      "cons" -> Seq(bool_sexp, " ", bool_sexp_list)))

  def bool = Rule(
    "bool", Map(
      "true" -> Seq("true"),
      "false" -> Seq("false")))
}
