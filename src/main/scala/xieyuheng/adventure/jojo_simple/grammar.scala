package xieyuheng.adventure.jojo_simple

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let", "exe",
    "assert_eq",
    "report_ds",
    "report_rs",
    "print",
    "ln",
    "cut",
  )

  def identifier = identifier_with_preserved("identifier", preserved)

  def jo: Rule = Rule(
    "jo", Map(
      "var" -> List(identifier),
      "let" -> List("(", "let", identifier, ":", ty_list, ")"),
      "jojo" -> List("{", jo_list, "}"),
      "jojo_empty" -> List("{", "}"),
      "claim" -> List(identifier, ":", "{", ty_list, "}"),
      "claim_empty" -> List(identifier, ":", "{", "}"),
      "define" -> List(identifier, "=", "{", jo_list, "}"),
      "define_empty" -> List(identifier, "=", "{", "}"),
      "exe" -> List("exe"),
      "assert_eq" -> List("assert_eq"),
      "report_ds" -> List("report_ds"),
      "report_rs" -> List("report_rs"),
      "print" -> List("print"),
      "ln" -> List("ln"),
    ))

  def jo_matcher: Tree => Jo = Tree.matcher[Jo](
    "jo", Map(
      "var" -> { case List(Leaf(name)) =>
        Var(name) },
      "let" -> { case List(_, _, Leaf(name), _, ty_list, _) =>
        Let(name, TyTy(ty_list_matcher(ty_list))) },
      "jojo" -> { case List(_, jo_list, _) =>
        JoJo(jo_list_matcher(jo_list)) },
      "jojo_empty" -> { case List(_, _) =>
        JoJo(List()) },
      "claim" -> { case List(Leaf(name), _, _, ty_list, _) =>
        Claim(name, TyTy(ty_list_matcher(ty_list))) },
      "claim_empty" -> { case List(Leaf(name), _, _, _) =>
        Claim(name, TyTy(List())) },
      "define" -> { case List(Leaf(name), _, _, jo_list, _) =>
        Define(name, JoJo(jo_list_matcher(jo_list))) },
      "define_empty" -> { case List(Leaf(name), _, _, _) =>
        Define(name, JoJo(List())) },
      "exe" -> { case List(_) =>
        Execute() },
      "assert_eq" -> { case List(_) =>
        AssertEq() },
      "report_ds" -> { case List(_) =>
        ReportDs() },
      "report_rs" -> { case List(_) =>
        ReportRs() },
      "print" -> { case List(_) =>
        Print() },
      "ln" -> { case List(_) =>
        Newline() },
    ))

  def jo_list = non_empty_list(jo)
  def jo_list_matcher = non_empty_list_matcher(jo_matcher)

  def ty: Rule = Rule(
    "ty", Map(
      "ty_atom" -> List(identifier),
      "tyty" -> List("{", ty_list, "}"),
      "ty_cut" -> List("cut"),
      "ty_minus" -> List("(", "-", ty, ")"),
    ))

  def ty_matcher: Tree => Ty = Tree.matcher[Ty](
    "ty", Map(
      "ty_atom" -> { case List(Leaf(name)) =>
        TyAtom(name) },
      "tyty" -> { case List(_, ty_list, _) =>
        TyTy(ty_list_matcher(ty_list)) },
      "ty_cut" -> { case List(_) =>
        TyCut() },
      "ty_minus" -> { case List(_, _, ty, _) =>
        TyMinus(ty_matcher(ty)) },
    ))

  def ty_list = non_empty_list(ty)
  def ty_list_matcher = non_empty_list_matcher(ty_matcher)

}
