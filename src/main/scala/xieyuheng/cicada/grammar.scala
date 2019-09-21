package xieyuheng.cicada

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let", "return",
    "data", "fn", "function",
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
      "let_type" -> List("let", identifier, ":", exp),
      "fn" -> List("fn", identifier, "(", non_empty_list(bind), ")", ":", exp, "=", exp),
      "fn_type" -> List("fn", identifier, "(", non_empty_list(bind), ")", ":", exp),
      "function" -> List("function", identifier, "(", non_empty_list(bind), ")", ":", exp, "=", exp),
      "function_type" -> List("function", identifier, "(", non_empty_list(bind), ")", ":", exp),
      "data" -> List("data", identifier,
        "(", non_empty_list(field), ")",
        "{", non_empty_list(member), "}"),
      "data_nullary" -> List("data", identifier,
        "{", non_empty_list(member), "}"),
      "class" -> List("class", identifier,
        "{", non_empty_list(decl), "}"),
      "class_extends" -> List("class", identifier,
        "extends", identifier,
        "{", non_empty_list(decl), "}"),
      "class_extends_many" -> List("class", identifier,
        "extends", identifier, non_empty_list(comma_identifier),
        "{", non_empty_list(decl), "}"),
      "class_empty_body" -> List("class", identifier,
        "{", "}"),
      "class_extends_empty_body" -> List("class", identifier,
        "extends", identifier,
        "{", "}"),
      "class_extends_many_empty_body" -> List("class", identifier,
        "extends", identifier, non_empty_list(comma_identifier),
        "{", "}"),
    ))

  def decl_matcher: Tree => Decl = Tree.matcher[Decl](
    "decl", Map(
      "let" -> { case List(_, Leaf(name), _, t, _, e) =>
        DeclLet(name, exp_matcher(t), exp_matcher(e)) },
      "let_type" -> { case List(_, Leaf(name), _, t) =>
        DeclLetType(name, exp_matcher(t)) },
      "fn" -> { case List(_, Leaf(name), _, bind_list, _, _, t, _, body) =>
        val args = non_empty_list_matcher(bind_matcher)(bind_list).toMap
        DeclFn(name, args, exp_matcher(t), exp_matcher(body)) },
      "fn_type" -> { case List(_, Leaf(name), _, bind_list, _, _, t) =>
        val args = non_empty_list_matcher(bind_matcher)(bind_list).toMap
        DeclFnType(name, args, exp_matcher(t)) },
      "function" -> { case List(_, Leaf(name), _, bind_list, _, _, t, _, body) =>
        val args = non_empty_list_matcher(bind_matcher)(bind_list).toMap
        DeclFn(name, args, exp_matcher(t), exp_matcher(body)) },
      "function_type" -> { case List(_, Leaf(name), _, bind_list, _, _, t) =>
        val args = non_empty_list_matcher(bind_matcher)(bind_list).toMap
        DeclFnType(name, args, exp_matcher(t)) },
      "data" -> { case List(_, Leaf(name), _, field_list, _, _, member_list, _) =>
        val fields = non_empty_list_matcher(field_matcher)(field_list)
        val members = non_empty_list_matcher(member_matcher(name))(member_list)
        DeclClub(name, members, fields) },
      "data_nullary" -> { case List(_, Leaf(name), _, member_list, _) =>
        val members = non_empty_list_matcher(member_matcher(name))(member_list)
        DeclClub(name, members, List()) },
      "class" -> { case List(_, Leaf(name),
        _, decl_list, _) =>
        val decls = non_empty_list_matcher(decl_matcher)(decl_list)
        DeclRecord(name, List(), decls) },
      "class_extends" -> { case List(_, Leaf(name),
        _, Leaf(super_name),
        _, decl_list, _) =>
        val decls = non_empty_list_matcher(decl_matcher)(decl_list)
        DeclRecord(name, List(super_name), decls) },
      "class_extends_many" -> { case List(_, Leaf(name),
          _, Leaf(super_name), rest_super_names,
        _, decl_list, _) =>
        val decls = non_empty_list_matcher(decl_matcher)(decl_list)
        val rest = non_empty_list_matcher(comma_identifier_matcher)(rest_super_names)
        DeclRecord(name, super_name +: rest, decls) },
      "class_empty_body" -> { case List(_, Leaf(name),
        _, _) =>
        val decls = List()
        DeclRecord(name, List(), decls) },
      "class_extends_empty_body" -> { case List(_, Leaf(name),
        _, Leaf(super_name),
        _, _) =>
        val decls = List()
        DeclRecord(name, List(super_name), decls) },
      "class_extends_many_empty_body" -> { case List(_, Leaf(name),
          _, Leaf(super_name), rest_super_names,
        _, _) =>
        val decls = List()
        val rest = non_empty_list_matcher(comma_identifier_matcher)(rest_super_names)
        DeclRecord(name, super_name +: rest, decls) },
    ))


  def comma_identifier = Rule(
    "comma_identifier", Map(
      "comma_identifier" -> List(",", identifier),
    ))

  def comma_identifier_matcher = Tree.matcher[String](
    "comma_identifier", Map(
      "comma_identifier" -> { case List(_, Leaf(name)) => name },
    ))


  def bind: Rule = Rule(
    "bind", Map(
      "nameless" -> List(exp),
      "nameless_comma" -> List(exp, ","),
      "named" -> List(identifier, ":", exp),
      "named_comma" -> List(identifier, ":", exp, ","),
    ))

  def bind_matcher = Tree.matcher[(String, Exp)](
    "bind", Map(
      "nameless" -> { case List(exp) =>
        ("_", exp_matcher(exp)) },
      "nameless_comma" -> { case List(exp, _) =>
        ("_", exp_matcher(exp)) },
      "named" -> { case List(Leaf(name), _, exp) =>
        (name, exp_matcher(exp)) },
      "named_comma" -> { case List(Leaf(name), _, exp, _) =>
        (name, exp_matcher(exp)) },
    ))


  def field: Rule = Rule(
    "field", Map(
      "type" -> List(identifier, ":", exp),
      "type_comma" -> List(identifier, ":", exp, ","),
      "value" -> List(identifier, ":", exp, "=", exp),
      "value_comma" -> List(identifier, ":", exp, "=", exp, ","),
    ))

  def field_matcher = Tree.matcher[(String, Exp, Option[Exp])](
    "field", Map(
      "type" -> { case List(Leaf(name), _, t) =>
        (name, exp_matcher(t), None) },
      "type_comma" -> { case List(Leaf(name), _, t, _) =>
        (name, exp_matcher(t), None) },
      "value" -> { case List(Leaf(name), _, t, _, body) =>
        (name, exp_matcher(t), Some(exp_matcher(body))) },
      "value_comma" -> { case List(Leaf(name), _, t, _, body, _) =>
        (name, exp_matcher(t), Some(exp_matcher(body))) },
    ))


  def member: Rule = Rule(
    "member", Map(
      "member" -> List("case", identifier, "(", non_empty_list(field), ")"),
      "member_nullary" -> List("case", identifier),
    ))


  def member_matcher(club_name: String) = Tree.matcher[Member](
    "member", Map(
      "member" -> { case List(_, Leaf(name), _, field_list, _) =>
        val fields = non_empty_list_matcher(field_matcher)(field_list)
        Member(name, club_name, fields) },
      "member_nullary" -> { case List(_, Leaf(name)) =>
        Member(name, club_name, List()) },
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
      // "the" -> List("the", "(", exp, ",", exp, ")"),
      "ap" -> List(rator, "(", non_empty_list(exp_comma), ")"),
      "ap_one" -> List(rator, "(", exp, ")"),
      "ap_drop" -> List(rator, "(", non_empty_list(exp_comma), exp, ")"),
      "choice" -> List("choice", path, "{", non_empty_list(choice_entry), "}"),
      "dot" -> List(exp, ".", identifier),
      "dot_type" -> List(exp, ".", ":", identifier),
      "block" -> List(block),
    ))

  def rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "rator", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      // "the" -> { case List(_, _, t, _, e, _) =>
      //   The(exp_matcher(t), exp_matcher(e)) },
      "ap" -> { case List(rator, _, exp_comma_list, _) =>
        non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) } },
      "ap_one" -> { case List(rator, _, exp, _) =>
        Ap(rator_matcher(rator), exp_matcher(exp)) },
      "ap_drop" -> { case List(rator, _, exp_comma_list, exp, _) =>
        val fn = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) }
        Ap(fn, exp_matcher(exp)) },
      "choice" -> { case List(_, path, _, choice_entry_list, _) =>
        val map = non_empty_list_matcher(choice_entry_matcher)(choice_entry_list).toMap
        Choice(path_matcher(path), map) },
      "dot" -> { case List(exp, _, Leaf(field_name)) =>
        Dot(exp_matcher(exp), field_name) },
      "dot_type" -> { case List(exp, _, _, Leaf(field_name)) =>
        DotType(exp_matcher(exp), field_name) },
      "block" -> { case List(block) => block_matcher(block) },
    ))

  def path: Rule = Rule(
    "path", Map(
      "name" -> List(identifier),
      "path" -> List(identifier, non_empty_list(dot_name)),
    ))

  def path_matcher = Tree.matcher[List[String]](
    "path", Map(
      "name" -> { case List(Leaf(name)) => List(name) },
      "path" -> { case List(Leaf(name), name_list) =>
        name +: non_empty_list_matcher(dot_name_matcher)(name_list) },
    ))


  def dot_name = Rule(
    "dot_name", Map(
      "dot_name" -> List(".", identifier),
    ))

  def dot_name_matcher = Tree.matcher[String](
    "dot_name", Map(
      "dot_name" -> { case List(_, Leaf(name)) => name },
    ))

  def choice_entry: Rule = Rule(
    "choice_entry", Map(
      "choice_entry" -> List("case", identifier, "=", ">", exp)
    ))

  def choice_entry_matcher = Tree.matcher[(String, Exp)](
    "choice_entry", Map(
      "choice_entry" -> { case List(_, Leaf(name), _, _, exp) =>
        (name, exp_matcher(exp)) }
    ))


  def block: Rule = Rule(
    "block", Map(
      "block" -> List("{", non_empty_list(decl), exp, "}"),
      "block_return" -> List("{", non_empty_list(decl), "return", exp, "}"),
      "block_one" -> List("{", exp, "}"),
    ))

  def block_matcher: Tree => Exp = Tree.matcher[Exp](
    "block", Map(
      "block" -> { case List(_, decl_list, exp, _) =>
        non_empty_list_matcher(decl_matcher)(decl_list)
          .foldRight(exp_matcher(exp)) { case (decl, body) =>
            Let(decl, body) } },
      "block_return" -> { case List(_, decl_list, _, exp, _) =>
        non_empty_list_matcher(decl_matcher)(decl_list)
          .foldRight(exp_matcher(exp)) { case (decl, body) =>
            Let(decl, body) } },
      "block_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
    ))


  def non_rator: Rule = Rule(
    "non_rator", Map(
      "type_t" -> List("type_t"),
      "type_level" -> List("type_t", "^", digit),
      "pi" -> List("(", non_empty_list(bind), ")", "-", ">", exp),
      "fn" -> List("(", non_empty_list(bind), ")", "=", ">", exp),
    ))

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      "type_t" -> { case _ => Type(1) },
      "type_level" -> { case List(_, _, Leaf(digit)) => Type(digit.toInt) },
      "pi" -> { case List(_, bind_list, _, _, _, dep_t) =>
        non_empty_list_matcher(bind_matcher)(bind_list)
          .foldRight(exp_matcher(dep_t)) {
            case ((name, arg_t), exp) => Pi(name, arg_t, exp) } },
      "fn" -> { case List(_, bind_list, _, _, _, body) =>
        non_empty_list_matcher(bind_matcher)(bind_list)
          .foldRight(exp_matcher(body)) {
            case ((name, arg_t), exp) => Fn(name, arg_t, exp) } },
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
