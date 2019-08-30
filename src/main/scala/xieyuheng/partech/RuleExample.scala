package xieyuheng.partech

import xieyuheng.partech.dsl._

object RuleExample {



  def non_empty_list(a: Rule)(implicit separater: String): Rule = Rule(
    s"list(${a.name})", Map(
      "unit" -> Seq(a),
      "cons" -> Seq(a, separater, non_empty_list(a))))


  def bool = Rule(
    "bool", Map(
      "true" -> Seq("true"),
      "false" -> Seq("false")))

  def bool_sexp = Rule(
    "bool_sexp", Map(
      "list" -> Seq("(", bool_sexp_list, ")"),
      "bool" -> Seq(bool)))

  // def bool_sexp_list: Rule = Rule(
  //   "bool_sexp_list", Map(
  //     "unit" -> Seq(bool_sexp),
  //     "cons" -> Seq(bool_sexp, " ", bool_sexp_list)))

  def bool_sexp_list: Rule = non_empty_list(bool_sexp)(" ")


  def tom_dick_and_harry = Rule(
    "tom_dick_and_harry", Map(
      "name" -> Seq(name),
      "list" -> Seq(name_list, " and ", name)))

  def name = Rule(
    "name", Map(
      "tom" -> Seq("tom"),
      "dick" -> Seq("dick"),
      "harry" -> Seq("harry")))

  // def name_list: Rule = Rule(
  //   "name_list", Map(
  //     "unit" -> Seq(name),
  //     "cons" -> Seq(name, ", ", name_list)))

  def name_list = non_empty_list(name)(", ")

  // regular grammar

  def tdh = Rule(
    "tdh", Map(
      "t" -> Seq("t"),
      "d" -> Seq("d"),
      "h" -> Seq("h"),
      "tdh_list" -> Seq(tdh_list)))

  def tdh_list = Rule(
    "tdh_list", Map(
      "t" -> Seq("t", tdh_list_tail),
      "d" -> Seq("d", tdh_list_tail),
      "h" -> Seq("h", tdh_list_tail)))

  def tdh_list_tail: Rule = Rule(
    "tdh_list_tail", Map(
      "list" -> Seq(",", tdh_list),
      "t" -> Seq("&t"),
      "d" -> Seq("&d"),
      "h" -> Seq("&h")))


  // left regular grammar

  def tdh_left = Rule(
    "tdh_left", Map(
      "t" -> Seq("t"),
      "d" -> Seq("d"),
      "h" -> Seq("h"),
      "list" -> Seq(tdh_left_list)))

  def tdh_left_list = Rule(
    "tdh_left_list", Map(
      "t" -> Seq(tdh_left_list_head, "&t"),
      "d" -> Seq(tdh_left_list_head, "&d"),
      "h" -> Seq(tdh_left_list_head, "&h")))

  def tdh_left_list_head: Rule = Rule(
    "tdh_left_list_head", Map(
      "t" -> Seq("t"),
      "d" -> Seq("d"),
      "h" -> Seq("h"),
      "before_t" -> Seq(tdh_left_list_head, ",t"),
      "before_d" -> Seq(tdh_left_list_head, ",d"),
      "before_h" -> Seq(tdh_left_list_head, ",h")))


  def sum: Rule = Rule(
    "sum", Map(
      "digit" -> Seq(digit),
      "sum" -> Seq(sum, " + ", sum)))

  def digit = Rule(
    "digit", Map(
      "0" -> Seq("0"),
      "1" -> Seq("1")))

}
