package xieyuheng.adventure.jojo_untyped

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let",
    "cons", "car", "cdr",
    "assert_eq",
    "report_ds",
    "report_rs",
  )

  def identifier = identifier_with_preserved("identifier", preserved)

  def jo: Rule = Rule(
    "jo", Map(
      "var" -> List(identifier),
      "let" -> List("(", "let", identifier. ")"),
      "jojo" -> List("{", jo_list, "}"),
      "jojo_empty" -> List("{", "}"),
      "define" -> List(identifier, "=", "{", jo_list, "}"),
      "define_empty" -> List(identifier, "=", "{", "}"),
      "string" -> List(double_quoted_string),
      "cons" -> List("cons"),
      "car" -> List("car"),
      "cdr" -> List("cdr"),
      "assert_eq" -> List("assert_eq"),
      "report_ds" -> List("report_ds"),
      "report_rs" -> List("report_rs"),
    ))

  def jo_list = non_empty_list(jo)
  def jo_list_matcher = non_empty_list_matcher(jo_matcher)

  def jo_matcher: Tree => Jo = Tree.matcher[Jo](
    "jo", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      "let" -> { case List(_, _, Leaf(name), _) => Let(name) },
      "jojo" -> { case List(_, jo_list, _) =>
        JoJo(jo_list_matcher(jo_list)) },
      "jojo_empty" -> { case List(_, _) => JoJo(List()) },
      "define" -> { case List(Leaf(name), _, _, jo_list, _) =>
        Define(name, JoJo(jo_list_matcher(jo_list))) },
      "define_empty" -> { case List(Leaf(name), _, _, _) =>
        Define(name, JoJo(List())) },
      "string" -> { case List(Leaf(str)) => Str(trim_double_quote(str)) },
      "cons" -> { case List(_) => Cons() },
      "car" -> { case List(_) => Car() },
      "cdr" -> { case List(_) => Cdr() },
      "assert_eq" -> { case List(_) => AssertEq() },
      "report_ds" -> { case List(_) => ReportDs() },
      "report_rs" -> { case List(_) => ReportRs() },
    ))

}
