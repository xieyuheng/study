package xieyuheng.simple

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let",
  )

  def identifier = identifier_with_preserved("identifier", preserved)

  def module = Rule(
    "module", Map(
      "module" -> List(non_empty_list(top)),
    ))

  def module_matcher = Tree.matcher[Module](
    "module", Map(
      "module" -> { case List(top_list) =>
        var module = Module()
        module.top_list = non_empty_list_matcher(top_matcher)(top_list)
        module
      },
    ))

  def top = Rule(
    "top", Map(
      "decl" -> List(decl),
      "show" -> List("@", "show", exp),
      "step" -> List("@", "step", exp),
      "walk_through" -> List("@", "walk_through", exp),
      "eq" -> List("@", "assert_eq", exp, exp),
      "not_eq" -> List("@", "assert_not_eq", exp, exp),
    ))

  def top_matcher = Tree.matcher[Top](
    "top", Map(
      "decl" -> { case List(decl) => TopDecl(decl_matcher(decl)) },
      "show" -> { case List(_, _, exp) => TopShow(exp_matcher(exp)) },
      "step" -> { case List(_, _, exp) => TopStep(exp_matcher(exp)) },
      "walk_through" -> { case List(_, _, exp) => TopWalkThrough(exp_matcher(exp)) },
      "eq" -> { case List(_, _, x, y) => TopEq(exp_matcher(x), exp_matcher(y)) },
      "not_eq" -> { case List(_, _, x, y) => TopNotEq(exp_matcher(x), exp_matcher(y)) },
    ))

  def decl = Rule(
    "decl", Map(
      "let" -> List("let", identifier, ":", ty, "=", exp),
    ))

  def decl_matcher = Tree.matcher[Decl](
    "decl", Map(
      "let" -> { case List(_, Leaf(name), _, t, _, e) =>
        DeclLet(name, ty_matcher(t), exp_matcher(e)) },
    ))

  def exp: Rule = Rule(
    "exp", Map(
      "rator" -> List(rator),
      "non_rator" -> List(non_rator),
    ))

  def exp_matcher: Tree => Exp = Tree.matcher[Exp](
    "exp", Map(
      "rator" -> { case List(rator) => rator_matcher(rator) },
      "non_rator" -> { case List(non_rator) => non_rator_matcher(non_rator) },
    ))

  def rator: Rule = Rule(
    "rator", Map(
      "var" -> List(identifier),
      "ap" ->
        List(rator, "(", non_empty_list(exp_comma), ")"),
      "ap_one" ->
        List(rator, "(", exp, ")"),
      "ap_drop" ->
        List(rator, "(", non_empty_list(exp_comma), exp, ")"),
    ))

  def rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "rator", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      "ap" -> { case List(rator, _, exp_comma_list, _) =>
        non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) } },
      "ap_one" -> { case List(rator, _, exp, _) =>
        Ap(rator_matcher(rator), exp_matcher(exp)) },
      "ap_drop" -> { case List(rator, _, exp_comma_list, exp, _) =>
        val fn = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) }
        Ap(fn, exp_matcher(exp)) },
    ))

  def arg_entry: Rule = Rule(
    "arg_entry", Map(
      "arg" -> List(identifier, ":", ty),
      "arg_comma" -> List(identifier, ":", ty, ","),
    ))

  def arg_entry_matcher = Tree.matcher[(String, Type)](
    "arg_entry", Map(
      "arg" -> { case List(Leaf(name), _, t) =>
        (name, ty_matcher(t)) },
      "arg_comma" -> { case List(Leaf(name), _, t, _) =>
        (name, ty_matcher(t)) },
    ))

  def ty: Rule = Rule(
    "ty", Map(
      "type_atom" -> List(identifier),
      "type_arrow" -> List("(", ty, ")", "-", ">", ty),
    ))

  def ty_matcher: Tree => Type = Tree.matcher[Type](
    "ty", Map(
      "type_atom" -> { case List(Leaf(name)) => TypeAtom(name) },
      "type_arrow" -> { case List(_, arg_t, _, _, _, ret_t) =>
        TypeArrow(ty_matcher(arg_t), ty_matcher(ret_t)) },
    ))

  def non_rator: Rule = Rule(
    "non_rator", Map(
      "fn" -> List("(", non_empty_list(arg_entry), ")", "=", ">", exp),
    ))

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      "fn" -> { case List(_, arg_entry_list, _, _, _, body) =>
        non_empty_list_matcher(arg_entry_matcher)(arg_entry_list)
          .foldRight(exp_matcher(body)) { case ((name, arg_t), exp) => Fn(name, arg_t, exp) } },
    ))

  def exp_comma = Rule(
    "exp_comma", Map(
      "exp_comma" -> List(exp, ","),
    ))

  def exp_comma_matcher = Tree.matcher[Exp](
    "exp_comma", Map(
      "exp_comma" -> { case List(exp, _) => exp_matcher(exp) },
    ))

}
