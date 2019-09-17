package xieyuheng.minitt

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let", "letrec",
    "car", "cdr",
    "type", "type_t",
    "case",
    "sole", "trivial",
    "return",
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

  def decl = Rule(
    "decl", Map(
      "let" -> List("let", pat, ":", exp, "=", exp),
      "letrec" -> List("letrec", pat, ":", exp, "=", exp),
    ))

  def decl_matcher = Tree.matcher[Decl](
    "decl", Map(
      "let" -> { case List(_, p, _, e, _, t) =>
        DeclLet(pat_matcher(p), exp_matcher(e), exp_matcher(t)) },
      "letrec" -> { case List(_, p, _, e, _, t) =>
        DeclLetrec(pat_matcher(p), exp_matcher(e), exp_matcher(t)) },
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

  def block: Rule = Rule(
    "block", Map(
      "block" -> List("{", non_empty_list(decl), "return", exp, "}"),
      "block_one" -> List("{", exp, "}"),
    ))

  def block_matcher: Tree => Exp = Tree.matcher[Exp](
    "block", Map(
      "block" -> { case List(_, decl_list, _, exp, _) =>
        non_empty_list_matcher(decl_matcher)(decl_list)
          .foldRight(exp_matcher(exp)) { case (decl, body) =>
            Block(decl, body) } },
      "block_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
    ))

  def rator: Rule = Rule(
    "rator", Map(
      "var" -> List(identifier),
      "ap" -> List(rator, "(", non_empty_list(exp_comma), ")"),
      "ap_one" -> List(rator, "(", exp, ")"),
      "ap_drop" -> List(rator, "(", non_empty_list(exp_comma), exp, ")"),
      "ap_block" -> List(rator, block),
      "block" -> List(block),
      "car" -> List("car", "(", exp, ")"),
      "cdr" -> List("cdr", "(", exp, ")"),
      "match" -> List("{", non_empty_list(mat_clause), "}"),
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
      "ap_block" -> { case List(rator, block) =>
        Ap(rator_matcher(rator), block_matcher(block)) },
      "block" -> { case List(block) => block_matcher(block) },
      "car" -> { case List(_, _, exp, _) => Car(exp_matcher(exp)) },
      "cdr" -> { case List(_, _, exp, _) => Cdr(exp_matcher(exp)) },
      "match" -> { case List(_, mat_clause_list, _) =>
        Mat(non_empty_list_matcher(mat_clause_matcher)(mat_clause_list).toMap) },
    ))

  def bind: Rule = Rule(
    "bind", Map(
      "bind" -> List(exp),
      "bind_comma" -> List(exp, ","),
      "bind_t" -> List(pat, ":", exp),
      "bind_t_comma" -> List(pat, ":", exp, ","),
    ))

  def bind_matcher = Tree.matcher[(Pat, Exp)](
    "bind", Map(
      "bind" -> { case List(exp) =>
        (PatSole(), exp_matcher(exp)) },
      "bind_comma" -> { case List(exp, _) =>
        (PatSole(), exp_matcher(exp)) },
      "bind_t" -> { case List(pat, _, exp) =>
        (pat_matcher(pat), exp_matcher(exp)) },
      "bind_t_comma" -> { case List(pat, _, exp, _) =>
        (pat_matcher(pat), exp_matcher(exp)) },
    ))

  def pat_entry: Rule = Rule(
    "pat_entry", Map(
      "arg" -> List(pat),
      "arg_comma" -> List(pat, ","),
    ))

  def pat_entry_matcher = Tree.matcher[Pat](
    "pat_entry", Map(
      "arg" -> { case List(pat) =>
        pat_matcher(pat) },
      "arg_comma" -> { case List(pat, _) =>
        pat_matcher(pat) },
    ))

  def non_rator: Rule = Rule(
    "non_rator", Map(
      "pi" -> List("(", non_empty_list(bind), ")", "-", ">", exp),
      "fn" -> List("(", non_empty_list(pat_entry), ")", "=", ">", exp),
      "fn_one" -> List(pat, "=", ">", exp),
      "cons" ->
        List("[", non_empty_list(exp_comma), "]"),
      "cons_one" ->
        List("[", exp, "]"),
      "cons_drop" ->
        List("[", non_empty_list(exp_comma), exp, "]"),
      "sigma" -> List("$", "[", non_empty_list(bind), exp, "]"),
      "data" -> List(identifier, exp),
      "sum" -> List("type", "{", non_empty_list(sum_clause), "}"),
      "sole" -> List("[", "]"),
      "lit_sole" -> List("sole"),
      "trivial" -> List("trivial"),
      "univ" -> List("type_t"),
    ))

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      "pi" -> { case List(_, bind_list, _, _, _, t) =>
        non_empty_list_matcher(bind_matcher)(bind_list)
          .foldRight(exp_matcher(t)) {
            case ((pat, arg_t), exp) => Pi(pat, arg_t, exp) } },
      "fn" -> { case List(_, pat_entry_list, _, _, _, t) =>
        non_empty_list_matcher(pat_entry_matcher)(pat_entry_list)
          .foldRight(exp_matcher(t)) {
            case (pat, exp) => Fn(pat, exp) } },
      "fn_one" -> { case List(pat, _, _, t) =>
        Fn(pat_matcher(pat), exp_matcher(t)) },
      "cons" -> { case List(_, exp_comma_list, _) =>
        val list = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
        list.init.foldRight(list.last) { case (head, tail) =>
          Cons(head, tail) } },
      "cons_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
      "cons_drop" -> { case List(_, exp_comma_list, exp, _) =>
        val list = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
        list.foldRight(exp_matcher(exp)) { case (head, tail) =>
          Cons(head, tail) } },
      "sigma" -> { case List(_, _, bind_list, t, _) =>
        non_empty_list_matcher(bind_matcher)(bind_list)
          .foldRight(exp_matcher(t)) {
            case ((pat, arg_t), exp) => Sigma(pat, arg_t, exp) } },
      "data" -> { case List(Leaf(tag), exp) =>
        Data(tag, exp_matcher(exp)) },
      "sum" -> { case List(_, _, sum_clause_list, _) =>
        Sum(non_empty_list_matcher(sum_clause_matcher)(sum_clause_list).toMap) },
      "sole" -> { case _ => Sole() },
      "lit_sole" -> { case _ => Sole() },
      "trivial" -> { case _ => Trivial() },
      "univ" -> { case _ => Univ() },
    ))

  def exp_comma = Rule(
    "exp_comma", Map(
      "exp_comma" -> List(exp, ","),
    ))

  def exp_comma_matcher = Tree.matcher[Exp](
    "exp_comma", Map(
      "exp_comma" -> { case List(exp, _) => exp_matcher(exp) },
    ))

  def sum_clause = Rule(
    "sum_clause", Map(
      "sum_trivial" -> List("case", identifier),
      "sum_clause" -> List("case", identifier, exp),
    ))

  def sum_clause_matcher: Tree => (String, Exp) = Tree.matcher[(String, Exp)](
    "sum_clause", Map(
      "sum_trivial" -> { case List(_, Leaf(name)) =>
        (name, Trivial())
      },
      "sum_clause" -> { case List(_, Leaf(name), t) =>
        (name, exp_matcher(t))
      },
    ))

  def mat_clause = Rule(
    "mat_clause", Map(
      "mat_clause" -> List("case", identifier, exp),
    ))

  def mat_clause_matcher: Tree => (String, Exp) = Tree.matcher[(String, Exp)](
    "mat_clause", Map(
      "mat_clause" -> { case List(_, Leaf(name), exp) =>
        (name, exp_matcher(exp)) },
    ))

  def pat: Rule = Rule(
    "pat", Map(
      "var" -> List(identifier),
      "cons" ->
        List("[", non_empty_list(pat_comma), "]"),
      "cons_one" ->
        List("[", pat, "]"),
      "cons_drop" ->
        List("[", non_empty_list(pat_comma), pat, "]"),
      "sole" -> List("[", "]"),
    ))

  def pat_matcher: Tree => Pat = Tree.matcher[Pat](
    "pat", Map(
      "var" -> { case List(Leaf(name)) => PatVar(name) },
      "cons" -> { case List(_, pat_comma_list, _) =>
        val list = non_empty_list_matcher(pat_comma_matcher)(pat_comma_list)
        list.init.foldRight(list.last) { case (head, tail) =>
          PatCons(head, tail) } },
      "cons_one" -> { case List(_, pat, _) =>
        pat_matcher(pat) },
      "cons_drop" -> { case List(_, pat_comma_list, pat, _) =>
        val list = non_empty_list_matcher(pat_comma_matcher)(pat_comma_list)
        list.foldRight(pat_matcher(pat)) { case (head, tail) =>
          PatCons(head, tail) } },
      "sole" -> { case _ => PatSole() },
    ))

  def pat_comma = Rule(
    "pat_comma", Map(
      "pat_comma" -> List(pat, ","),
    ))

  def pat_comma_matcher = Tree.matcher[Pat](
    "pat_comma", Map(
      "pat_comma" -> { case List(pat, _) => pat_matcher(pat) },
    ))
}
