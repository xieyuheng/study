package xieyuheng.mini_tt

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  val sentences = List(
    s"""
    let id: (A: U) -> A = x => x
    """,

    s"""
    let bool_t: U = sum {
      true[];
      false[];
    }
    """,

    s"""
    let bool_elim:
      (C: (_: bool_t) -> U) ->
      (h0: C(true)) ->
      (h1: C(false)) ->
      (b: bool_t) -> C(b) =
    C => h0 => h1 => match {
      true[] => h0;
      false[] => h1;
    }
    """,


    s"""
    letrec nat_t: U = sum {
      zero[];
      succ[nat_t];
    }
    """,

    s"""
    let zero: nat_t = zero[]
    """,

    s"""
    let one: nat_t = succ[zero]
    """,

    s"""
    let two: nat_t = succ[one]
    """,

    s"""
    let three: nat_t = succ[two]
    """,

    s"""
    let four: nat_t = succ[three]
    """,

    s"""
    let five: nat_t = succ[four]
    """,

    s"""
    let six: nat_t = succ[five]
    """,

    s"""
    let seven: nat_t = succ[six]
    """,

    s"""
    let eight: nat_t = succ[seven]
    """,

    s"""
    let nine: nat_t = succ[eight]
    """,

    s"""
    let ten: nat_t = succ[nine]
    """,

    s"""
    letrec nat_rec:
      (C: (_: nat_t) -> U) ->
      (a: C(zero)) ->
      (g: (n: nat_t) -> (_: C(n)) -> C(succ[n])) ->
      (n: nat_t) -> C(n) =
    C => a => g => match {
      zero [] => a;
      succ [prev] => g(prev, nat_rec(C, a, g, prev));
    }
    """,

    s"""
    letrec add: (x: nat_t) -> (y: nat_t) -> nat_t = match {
      zero[] => y => y;
      succ[prev] => y => succ(add(prev, y));
    }
    """,

    s"""
    letrec nat_eq: (x: nat_t) -> (y: nat_t) -> bool_t =
    match {
      zero[] => match {
        zero[] => true;
        succ[_] => false;
      };
      succ[x_prev] => match {
        zero[] => false;
        succ[y_prev] => nat_eq(x_prev, y_prev);
      };
    }
    """,

    s"""
    letrec _: _ = [A , list_t(A) ,]
    """,

    s"""
    letrec list_t: (A: U) -> U =
    A => sum {
      nil[];
      cons[A, list_t(A)];
    }
    """,

    s"""
    letrec list_append: (A: U) -> (x: list_t(A)) -> (y: list_t(A)) -> list_t(A) =
    A => match {
      nil[] => y => y;
      cons[head, tail] => y => cons[head, list_append(A, tail, y)];
    }
    """,
  )

  val non_sentences = List(
    "",
  )

  def start = decl

  def preservedIdentifiers: Set[String] = Set(
    "let", "letrec",
    "sum", "match",
    "trivial",
    "U",
  )

  def identifier: WordPred = WordPred(
    "identifier", { case word =>
      if (preservedIdentifiers.contains(word)) {
        false
      } else {
        word.headOption match {
          case Some(char) =>
            val head_set = lower_case_char_set ++ upper_case_char_set + '_'
            val tail_set = head_set ++ digit_char_set
            head_set.contains(char) && wordInCharSet(tail_set)(word.tail)
          case None => false
        }
      }
    })

  def decl = Rule("decl", Map(
    "let" -> List("let", pattern, ":", exp, "=", exp),
    "letrec" -> List("letrec", pattern, ":", exp, "=", exp),
  ))

  def exp: Rule = Rule("exp", Map(
    "rator" -> List(rator),
    "non_rator" -> List(non_rator),
  ))

  def non_rator: Rule = Rule("non_rator", Map(
    "pi" -> List("(", pattern, ":", exp, ")", "-", ">", exp),
    "fn" -> List(pattern, "=", ">", exp),
    "cons" ->
      List("[", non_empty_list(exp_comma), "]"),
    "cons_one_without_comma" ->
      List("[", exp, "]"),
    "cons_without_last_comma" ->
      List("[", non_empty_list(exp_comma), exp, "]"),
    "sigma" -> List("(", pattern, ":", exp, ")", "*", "*", exp),
    "data" -> List(identifier, exp),
    "sum" -> List("sum", "{", non_empty_list(clause), "}"),
    "sole" -> List("[", "]"),
    "trivial" -> List("trivial"),
    "U" -> List("U"),
  ))

  def rator: Rule = Rule("rator", Map(
    "var" -> List(identifier),
    "ap" ->
      List(rator, "(", non_empty_list(exp_comma), ")"),
    "ap_one_without_comma" ->
      List(rator, "(", exp, ")"),
    "ap_without_last_comma" ->
      List(rator, "(", non_empty_list(exp_comma), exp, ")"),
    "car" -> List(exp, ".", "car"),
    "cdr" -> List(exp, ".", "cdr"),
    "match" -> List("match", "{", non_empty_list(clause), "}"),
  ))

  def exp_comma = Rule("exp_comma", Map(
    "exp_comma" -> List(exp, ","),
  ))

  def clause = Rule("clause", Map(
    "clause_with_comma" -> List(identifier, exp, ";"),
  ))

  def pattern: Rule = Rule("pattern", Map(
    "var" -> List(identifier),
    "cons" ->
      List("[", non_empty_list(pattern_comma), "]"),
    "cons_one_without_comma" ->
      List("[", pattern, "]"),
    "cons_without_last_comma" ->
      List("[", non_empty_list(pattern_comma), pattern, "]"),
    "sole" -> List("[", "]"),
  ))

  def pattern_comma = Rule("pattern_comma", Map(
    "pattern_comma" -> List(pattern, ","),
  ))

  implicit def treeToDecl: TreeTo[Decl] = TreeTo[Decl] { case tree =>
    tree match {
      case Node(Rule("decl", _, _), "let", List(p, _, e, _, t)) =>
        Let(treeToPattern(p), treeToExp(e), treeToExp(t))
      case Node(Rule("decl", _, _), "letrec", List(p, _, e, _, t)) =>
        Letrec(treeToPattern(p), treeToExp(e), treeToExp(t))
      case _ => throw new Exception()
    }
  }

  implicit def treeToExp: TreeTo[Exp] = TreeTo[Exp] { case tree =>
    tree match {
      case Node(Rule("exp", _, _), "rator", List(rator)) =>
        ???
      case Node(Rule("exp", _, _), "non_rator", List(exp)) =>
        ???
      case _ => throw new Exception()
    }
  }

  implicit def treeToPattern: TreeTo[Pattern] = TreeTo[Pattern] { case tree =>
    tree match {
      case Node(Rule("pattern", _, _), "head_a", List(_, b)) =>
        ???
      case _ => throw new Exception()
    }
  }

}
