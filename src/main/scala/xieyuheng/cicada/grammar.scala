package xieyuheng.cicada

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let", "return",
    "data", "fn",
    "the", "type_t",
    "case", "choice",
    "class", "extends",
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
      "eval" -> List("eval", "!", exp),
      "eq" -> List("eq", "!", exp, exp),
      "not_eq" -> List("not_eq", "!", exp, exp),
    ))

  def top_matcher = Tree.matcher[Top](
    "top", Map(
      "decl" -> { case List(decl) => TopDecl(decl_matcher(decl)) },
      "eval" -> { case List(_, _, exp) => TopEval(exp_matcher(exp)) },
      "eq" -> { case List(_, _, x, y) => TopEq(exp_matcher(x), exp_matcher(y)) },
      "not_eq" -> { case List(_, _, x, y) => TopNotEq(exp_matcher(x), exp_matcher(y)) },
    ))

  def decl: Rule = Rule(
    "decl", Map(
      "let" -> List("let", identifier, ":", exp, "=", exp),
      "fn" -> List("fn", identifier, "(", non_empty_list(bind), ")", ":", exp, "=", exp),
      "data" -> List("data", identifier,
        "(", non_empty_list(field), ")",
        "{", non_empty_list(member), "}"),
    ))


  def decl_matcher = Tree.matcher[Decl](
    "decl", Map(
      // TODO
    ))

  def bind: Rule = Rule(
    "bind", Map(
      "nameless" -> List(exp),
      "nameless_comma" -> List(exp, ","),
      "named" -> List(identifier, ":", exp),
      "named_comma" -> List(identifier, ":", exp, ","),
    ))

  def field: Rule = Rule(
    "field", Map(
      "type" -> List(identifier, ":", exp),
      "type_comma" -> List(identifier, ":", exp, ","),
      "value" -> List(identifier, ":", exp, "=", exp),
      "value_comma" -> List(identifier, ":", exp, "=", exp, ","),
    ))

  def member: Rule = Rule(
    "member", Map(
      "member" -> List("case", identifier, "(", non_empty_list(field), ")"),
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
      // final case class The(t: Exp, body: Exp) extends Exp
      "the" -> List("the", "(", exp, ",", exp, ")"),
      // final case class Ap(target: Exp, arg: Exp) extends Exp
      // final case class Choice(target: Exp, map: Map[String, Exp]) extends Exp
      // final case class Dot(target: Exp, field_name: String) extends Exp
      // final case class DotType(target: Exp, field_name: String) extends Exp
      // final case class Let(decl: Decl, body: Exp) extends Exp
    ))

  def rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "rator", Map(
      // TODO
    ))

  def non_rator: Rule = Rule(
    "non_rator", Map(
      // final case class Type(level: Int) extends Exp
      "type_t" -> List("type_t"),
      // final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
      // final case class Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
    ))

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      // TODO
    ))
}
