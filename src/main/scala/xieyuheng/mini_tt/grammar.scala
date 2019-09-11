package xieyuheng.mini_tt

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  val sentences = List(
    s"""
    let a: A = x
    """,

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
    let x: (_: X) -> U = A => cons[A, list_t(A), ]
    """,

    s"""
    let x: X = sum {
      nil[];
    }
    """,

    s"""
    let x: X = sum {
      cons[A, list_t(A), ];
    }
    """,

    // TODO
    // can not parse

    // problem is about non_empty_list(clause)
    // we need to test non_empty_list in partech

    // s"""
    // letrec x: X = sum {
    //   nil[];
    //   cons[A, list_t(A), ];
    // }
    // """,

    // TODO
    // can not treeTo

    // s"""
    // letrec list_t: (A: U) -> U =
    // A => sum {
    //   nil[];
    //   cons[A, list_t(A)];
    // }
    // """,

    // s"""
    // letrec list_append: (A: U) -> (x: list_t(A)) -> (y: list_t(A)) -> list_t(A) =
    // A => match {
    //   nil[] => y => y;
    //   cons[head, tail] => y => cons[head, list_append(A, tail, y)];
    // }
    // """,
  )

  val non_sentences = List(
    "",
  )

  def start = decl

  def preserved_identifiers: Set[String] = Set(
    "let", "letrec",
    "sum", "match",
    "trivial",
    "U",
  )

  def identifier: WordPred = WordPred(
    "identifier", { case word =>
      if (preserved_identifiers.contains(word)) {
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

  def decl = Rule(
    "decl", Map(
      "let" -> List("let", pattern, ":", exp, "=", exp),
      "letrec" -> List("letrec", pattern, ":", exp, "=", exp),
    ))

  def decl_matcher = Tree.matcher[Decl](
    "decl", Map(
      "let" -> { case List(_, p, _, e, _, t) =>
        Let(pattern_matcher(p), exp_matcher(e), exp_matcher(t)) },
      "letrec" -> { case List(_, p, _, e, _, t) =>
        Letrec(pattern_matcher(p), exp_matcher(e), exp_matcher(t)) },
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
      "ap_one_without_comma" ->
        List(rator, "(", exp, ")"),
      "ap_without_last_comma" ->
        List(rator, "(", non_empty_list(exp_comma), exp, ")"),
      "car" -> List(exp, ".", "car"),
      "cdr" -> List(exp, ".", "cdr"),
      "match" -> List("match", "{", non_empty_list(clause), "}"),
    ))

  def rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "rator", Map(
      "var" -> { case List(Leaf(name)) => Var(name) },
      "ap" -> { case List(rator, _, exp_comma_list, _) =>
        non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) } },
      "ap_one_without_comma" -> { case List(rator, _, exp, _) =>
        Ap(rator_matcher(rator), exp_matcher(exp)) },
      "ap_without_last_comma" -> { case List(rator, _, exp_comma_list, exp, _) =>
        val fn = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
          .foldLeft(rator_matcher(rator)) { case (fn, arg) => Ap(fn, arg) }
        Ap(fn, exp_matcher(exp)) },
      "car" -> { case List(exp, _, _) => Car(exp_matcher(exp)) },
      "cdr" -> { case List(exp, _, _) => Cdr(exp_matcher(exp)) },
      "match" -> { case List(_, _, clause_list, _) =>
        Mat(non_empty_list_matcher(clause_matcher)(clause_list).toMap) },
    ))

  def non_rator: Rule = Rule(
    "non_rator", Map(
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

  def non_rator_matcher: Tree => Exp = Tree.matcher[Exp](
    "non_rator", Map(
      "pi" -> { case List(_, pattern, _, argType, _, _, _, t) =>
        Pi(pattern_matcher(pattern), exp_matcher(argType), exp_matcher(t)) },
      "fn" -> { case List(pattern, _, _, exp) =>
        Fn(pattern_matcher(pattern), exp_matcher(exp)) },
      "cons" -> { case List(_, exp_comma_list, _) =>
        val list = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
        list.init.foldRight(list.last) { case (tail, head) =>
          Cons(head, tail) } },
      "cons_one_without_comma" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
      "cons_without_last_comma" -> { case List(_, exp_comma_list, exp, _) =>
        val list = non_empty_list_matcher(exp_comma_matcher)(exp_comma_list)
        list.foldRight(exp_matcher(exp)) { case (tail, head) =>
          Cons(head, tail) } },
      "sigma" -> { case List(_, pattern, _, argType, _, _, _, t) =>
        Sigma(pattern_matcher(pattern), exp_matcher(argType), exp_matcher(t)) },
      "data" -> { case List(Leaf(tag), exp) =>
        Data(tag, exp_matcher(exp)) },
      "sum" -> { case List(_, _, clause_list, _) =>
        Sum(non_empty_list_matcher(clause_matcher)(clause_list).toMap) },
      "sole" -> { case _ => Sole },
      "trivial" -> { case _ => Trivial },
      "U" -> { case _ => U },
    ))

  def exp_comma = Rule(
    "exp_comma", Map(
      "exp_comma" -> List(exp, ","),
    ))

  def exp_comma_matcher = Tree.matcher[Exp](
    "exp_comma", Map(
      "exp_comma" -> { case List(exp, _) => exp_matcher(exp) },
    ))

  def clause = Rule(
    "clause", Map(
      "clause" -> List(identifier, exp, ";"),
    ))

  def clause_matcher: Tree => (String, Exp) = Tree.matcher[(String, Exp)](
    "clause", Map(
      "clause" -> { case List(Leaf(name), exp, _) =>
        (name, exp_matcher(exp)) },
    ))

  def pattern: Rule = Rule(
    "pattern", Map(
      "var" -> List(identifier),
      "cons" ->
        List("[", non_empty_list(pattern_comma), "]"),
      "cons_one_without_comma" ->
        List("[", pattern, "]"),
      "cons_without_last_comma" ->
        List("[", non_empty_list(pattern_comma), pattern, "]"),
      "sole" -> List("[", "]"),
    ))

  def pattern_matcher: Tree => Pattern = Tree.matcher[Pattern](
    "pattern", Map(
      "var" -> { case List(Leaf(name)) => VarPattern(name) },
      "cons" -> { case List(_, pattern_comma_list, _) =>
        val list = non_empty_list_matcher(pattern_comma_matcher)(pattern_comma_list)
        list.init.foldRight(list.last) { case (tail, head) =>
          ConsPattern(head, tail) } },
      "cons_one_without_comma" -> { case List(_, pattern, _) =>
        pattern_matcher(pattern) },
      "cons_without_last_comma" -> { case List(_, pattern_comma_list, pattern, _) =>
        val list = non_empty_list_matcher(pattern_comma_matcher)(pattern_comma_list)
        list.foldRight(pattern_matcher(pattern)) { case (tail, head) =>
          ConsPattern(head, tail) } },
      "sole" -> { case _ => SolePattern },
    ))

  def pattern_comma = Rule(
    "pattern_comma", Map(
      "pattern_comma" -> List(pattern, ","),
    ))

  def pattern_comma_matcher = Tree.matcher[Pattern](
    "pattern_comma", Map(
      "pattern_comma" -> { case List(pattern, _) => pattern_matcher(pattern) },
    ))
}
