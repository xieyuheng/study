package xieyuheng.tartlet

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "let", "return",
    "the",
    "eqv_t", "replace", "same",
    "nat_t", "zero", "succ", "nat_ind",
    "absurd_t", "absurd_ind",
    "cons", "car", "cdr",
    "trivial_t", "sole",
    "type_t",
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
      "let" -> List("let", identifier, ":", exp, "=", exp),
    ))

  def decl_matcher = Tree.matcher[Decl](
    "decl", Map(
      "let" -> { case List(_, Leaf(name), _, t, _, e) =>
        DeclLet(name, exp_matcher(t), exp_matcher(e)) },
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
      "ap" -> List(rator, "(", non_empty_list(exp_comma), ")"),
      "ap_drop" -> List(rator, "(", non_empty_list(exp_comma), exp, ")"),
      "ap_one" -> List(rator, "(", exp, ")"),
      // "ap_to_block" -> List(rator, block),
      "the" -> List("the", "(", exp, ",", exp, ")"),
      "zero" -> List("zero"),
      "succ" -> List("succ", "(", exp, ")"),
      "nat_ind" -> List("nat_ind", "(", exp, ",", exp, ",", exp, ",", exp, ")"),
      // "block" -> List(block),
    ))

  def rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "rator", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      "ap" -> { case List(rator, _, exp_comma_list, _) =>
        non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) } },
      "ap_drop" -> { case List(rator, _, exp_comma_list, exp, _) =>
        val fn = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) }
        Ap(fn, exp_matcher(exp)) },
      "ap_one" -> { case List(rator, _, exp, _) =>
        Ap(rator_matcher(rator), exp_matcher(exp)) },
      // "ap_to_block" -> { case List(rator, block) =>
      //   Ap(rator_matcher(rator), block_matcher(block)) },
      "the" -> { case  List(_, _, t, _, e, _) => The(exp_matcher(t), exp_matcher(e)) },
      "zero" -> { case _ => Zero() },
      "succ" -> { case List(_, _, exp, _) => Succ(exp_matcher(exp)) },
      "nat_rec" -> { case List(_, _, target, _, motive, _, base, _, step, _) =>
        NatInd(exp_matcher(target), exp_matcher(motive), exp_matcher(base), exp_matcher(step)) },
      // "block" -> { case List(block) => block_matcher(block) },
    ))

  // def block: Rule = Rule(
  //   "block", Map(
  //     "block" -> List("{", non_empty_list(decl), "return", exp, "}"),
  //     "block_one" -> List("{", exp, "}"),
  //   ))

  // def block_matcher: Tree => Exp = Tree.matcher[Exp](
  //   "block", Map(
  //     "block" -> { case List(_, decl_list, _, exp, _) =>
  //       non_empty_list_matcher(decl_matcher)(decl_list)
  //         .foldRight(exp_matcher(exp)) { case (decl, body) =>
  //           Block(decl, body) } },
  //     "block_one" -> { case List(_, exp, _) =>
  //       exp_matcher(exp) },
  //   ))

  def id_entry: Rule = Rule(
    "id_entry", Map(
      "arg" -> List(identifier),
      "arg_comma" -> List(identifier, ","),
    ))

  def id_entry_matcher = Tree.matcher[String](
    "id_entry", Map(
      "arg" -> { case List(Leaf(name)) =>
        name },
      "arg_comma" -> { case List(Leaf(name), _) =>
        name },
    ))

  def non_rator: Rule = Rule(
    "non_rator", Map(
      "fn" -> List(identifier, "=", ">", exp),
      "multi_fn" -> List("(", non_empty_list(id_entry), ")", "=", ">", exp),
    ))

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      "fn" -> { case List(Leaf(name), _, _, body) =>
        Fn(name, exp_matcher(body)) },
      "multi_fn" -> { case List(_, id_entry_list, _, _, _, body) =>
        var exp = exp_matcher(body)
        non_empty_list_matcher(id_entry_matcher)(id_entry_list)
          .reverse.foreach { case pat =>
            exp = Fn(pat, exp)
          }
        exp },
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
