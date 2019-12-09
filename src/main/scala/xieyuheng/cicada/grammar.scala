package xieyuheng.cicada

import collection.immutable.ListMap

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "type",
    "class", "define",
    "given", "conclude",
    "let", "return",
  )

  def identifier = identifier_with_preserved("identifier", preserved)

  def exp: Rule = Rule(
    "exp", Map(
      "var" -> List(identifier),
      "type" -> List("type"),
      "pi" -> List("{", "given", identifier, ":", exp, "conclude", exp, "}"),
      "fn" -> List("{", "given", identifier, ":", exp, "return", exp, "}"),
      "ap" -> List(exp, "(", exp, ")"),
      "cl" -> List("class", "{", non_empty_list(given_entry), "}"),
      "cl_empty" -> List("class", "{", "}"),
      "cl_naked" -> List("{", non_empty_list(given_entry), "}"),
      "obj" -> List("object", "{", non_empty_list(let_entry), "}"),
      "obj_empty" -> List("object", "{", "}"),
      "obj_naked" -> List("{", non_empty_list(let_entry), "}"),
      "dot" -> List(exp, ".", identifier),
    ))

  def exp_matcher: Tree => Exp = Tree.matcher[Exp](
    "exp", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      "type" -> { case List(_) => Type() },
      "pi" -> { case List(_, _, Leaf(arg_name), _, arg_type, _, ret_type, _) =>
        Pi(arg_name, exp_matcher(arg_type), exp_matcher(ret_type)) },
      "fn" -> { case List(_, _, Leaf(arg_name), _, arg_type, _, body, _) =>
        Fn(arg_name, exp_matcher(arg_type), exp_matcher(body)) },
      "ap" -> { case List(target, _, arg, _) =>
        Ap(exp_matcher(target), exp_matcher(arg)) },
      "cl" -> { case List(_, _, given_entry_list, _) =>
        Cl(ListMap(non_empty_list_matcher(given_entry_matcher)(given_entry_list) : _*)) },
      "cl_empty" -> { case List(_, _, _) =>
        Cl(ListMap()) },
      "cl_naked" -> { case List(_, given_entry_list, _) =>
        Cl(ListMap(non_empty_list_matcher(given_entry_matcher)(given_entry_list) : _*)) },
      "obj" -> { case List(_, _, let_entry_list, _) =>
        Obj(ListMap(non_empty_list_matcher(let_entry_matcher)(let_entry_list) : _*)) },
      "obj_empty" -> { case List(_, _, _) =>
        Obj(ListMap()) },
      "obj_naked" -> { case List(_, let_entry_list, _) =>
        Obj(ListMap(non_empty_list_matcher(let_entry_matcher)(let_entry_list) : _*)) },
      "dot" -> { case List(target, _, Leaf(field)) =>
        Dot(exp_matcher(target), field) }
    ))

  def given_entry = Rule(
    "given_entry", Map(
      "given" -> List("given", identifier, ":", exp),
    ))

  def given_entry_matcher = Tree.matcher[(String, Exp)](
    "given_entry", Map(
      "given" -> { case List(_, Leaf(name), _, exp) => (name, exp_matcher(exp)) },
    ))

  def let_entry = Rule(
    "let_entry", Map(
      "let" -> List("let", identifier, "=", exp),
    ))

  def let_entry_matcher = Tree.matcher[(String, Exp)](
    "let_entry", Map(
      "let" -> { case List(_, Leaf(name), _, exp) => (name, exp_matcher(exp)) },
    ))

}
