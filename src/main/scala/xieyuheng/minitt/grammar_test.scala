package xieyuheng.minitt

import xieyuheng.partech._
import xieyuheng.partech.pretty._
import xieyuheng.partech.example._

import xieyuheng.minitt.expDSL._

object grammar_test extends App {

  val decl_sentences = List(
    s"""
    let a: A = x
    """,

    s"""
    let id: (A: type_t) -> (_: A) -> A =
    A => x => x
    """,

    s"""
    let bool_t: type_t = type {
      case true
      case false
    }
    """,

    s"""
    let bool_elim:
      (C: (_: bool_t) -> type_t) ->
      (h0: C(true)) ->
      (h1: C(false)) ->
      (b: bool_t) -> C(b) =
    C => h0 => h1 => {
      case true[] => h0
      case false[] => h1
    }
    """,

    s"""
    let bool_elim:
      (C: (_: bool_t) -> type_t) ->
      (h0: C(true), h1: C(false)) ->
      (b: bool_t) -> C(b) =
    C => h0 => h1 => {
      case true[] => h0
      case false[] => h1
    }
    """,

    s"""
    letrec nat_t: type_t = type {
      case zero
      case succ[nat_t]
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
      (C: (_: nat_t) -> type_t) ->
      (a: C(zero)) ->
      (g: (n: nat_t) -> (_: C(n)) -> C(succ[n])) ->
      (n: nat_t) -> C(n) =
    C => a => g => {
      case zero [] => a
      case succ [prev] => g(prev, nat_rec(C, a, g, prev))
    }
    """,

    s"""
    letrec add: (x: nat_t) -> (y: nat_t) -> nat_t = {
      case zero[] => y => y
      case succ[prev] => y => succ[add(prev, y)]
    }
    """,

    s"""
    letrec nat_eq: (x: nat_t) -> (y: nat_t) -> bool_t =
    {
      case zero[] => {
        case zero[] => true
        case succ[_] => false
      }
      case succ[x_prev] => {
        case zero[] => false
        case succ[y_prev] => nat_eq(x_prev, y_prev)
      }
    }
    """,

    s"""
    letrec _: _ = [A , list_t(A) ,]
    """,

    s"""
    let x: (_: X) -> type_t = A => cons[A, list_t(A), ]
    """,

    s"""
    let x: X = type {
      case nil
    }
    """,

    s"""
    let x: X = type {
      case nil
      case nil2
    }
    """,

    s"""
    let x: X = type {
      case nil
      case nil2
      case nil3
    }
    """,

    s"""
    let x: X = type {
      case cons (_: A) ** list_t(A)
    }
    """,

    s"""
    let x: X = type {
      case cons (_: A) ** list_t(A)
      case cons2 (_: A) ** list_t(A)
    }
    """,

    s"""
    let x: X = type {
      case nil
      case cons (_: A) ** list_t(A)
    }
    """,

    s"""
    letrec list_t: type_t = type {
      case nil
      case cons $$[A, list_t(A)]
    }
    """,

    s"""
    letrec list_append: (A: type_t) -> (x: list_t(A)) -> (y: list_t(A)) -> list_t(A) =
    A => {
      case nil[] => y => y
      case cons[head, tail] => y => cons[head, list_append(A, tail, y)]
    }
    """,
  )

  val test_parse_decl_sentences = {
    decl_sentences.foreach { case text =>
      Parser(grammar.lexer, grammar.decl).parse(text) match {
        case Right(tree) => {}
        case Left(error) =>
          println(s"[test_parse_decl_sentences] should parse")
          println(s"- text: ${text}")
          println(s"- error: ${error}")
          throw new Exception()
      }
    }
  }

  def assert_decl_to_tree(text: String, decl: Decl): Unit = {
    Parser(grammar.lexer, grammar.decl).parse(text) match {
      case Right(tree) =>
        if (grammar.decl_matcher(tree) != decl) {
          println(s"[assert_decl_to_tree] assertion fail")
          println(s"- text: ${text}")
          println(s"- expect: ${decl}")
          println(s"- actual: ${grammar.decl_matcher(tree)}")
          throw new Exception()
        }
      case Left(error) =>
        println(s"[assert_decl_to_tree] parse error")
        println(s"- text: ${text}")
        println(s"- decl: ${decl}")
        println(s"- error: ${error}")
        throw new Exception()
    }
  }

  val test_decl_to_tree = {
    assert_decl_to_tree(
      s"""
      let id: (A: type_t, A) -> A =
      A => x => x
      """,
      DeclLet("id",
        pi("A" :: Univ()) { "A" ->: "A" },
        fn("A", "x") { "x" })
    )

    assert_decl_to_tree(
      s"""
      let bool_t: type_t = type {
        case true
        case false
      }
      """,
      DeclLet("bool_t", Univ(),
        sum(
          "true" -> Trivial(),
          "false" -> Trivial()))
    )

    assert_decl_to_tree(
      s"""
      let true: bool_t = true[]
      """,
      DeclLet("true", "bool_t", %("true"))
    )

    assert_decl_to_tree(
      s"""
      let false: bool_t = false[]
      """,
      DeclLet("false", "bool_t", %("false"))
    )

    assert_decl_to_tree(
      s"""
      let bool_elim:
        (C: (bool_t) -> type_t) ->
        (h0: C(true)) ->
        (h1: C(false)) ->
        (b: bool_t) -> C(b) =
      C => h0 => h1 => {
        case true[] => h0
        case false[] => h1
      }
      """,
      DeclLet("bool_elim",
        pi("C" :: "bool_t" ->: Univ()) {
          pi("h0" :: ("C" $ "true")) {
            pi("h1" :: ("C" $ "false")) {
              pi("b" :: "bool_t") { "C" $ "b" }
            } } },
        fn("C", "h0", "h1") { mat(
          "true" -> fn(__) { "h0" },
          "false" -> fn(__) { "h1" }) })
    )

    assert_decl_to_tree(
      s"""
      let bool_elim:
        (C: (bool_t) -> type_t) ->
        (h0: C(true), h1: C(false)) ->
        (b: bool_t) -> C(b) =
      C => h0 => h1 => {
        case true[] => h0
        case false[] => h1
      }
      """,
      DeclLet("bool_elim",
        pi("C" :: "bool_t" ->: Univ()) {
          pi("h0" :: ("C" $ "true")) {
            pi("h1" :: ("C" $ "false")) {
              pi("b" :: "bool_t") { "C" $ "b" }
            } } },
        fn("C", "h0", "h1") { mat(
          "true" -> fn(__) { "h0" },
          "false" -> fn(__) { "h1" }) })
    )

    assert_decl_to_tree(
      s"""
      let bool_elim: (
        C: (bool_t) -> type_t,
        h0: C(true),
        h1: C(false),
        b: bool_t,
      ) -> C(b) = C => h0 => h1 => {
        case true[] => h0
        case false[] => h1
      }
      """,
      DeclLet("bool_elim",
        pi("C" :: "bool_t" ->: Univ()) {
          pi("h0" :: ("C" $ "true")) {
            pi("h1" :: ("C" $ "false")) {
              pi("b" :: "bool_t") { "C" $ "b" }
            } } },
        fn("C", "h0", "h1") { mat(
          "true" -> fn(__) { "h0" },
          "false" -> fn(__) { "h1" }) })
    )

    assert_decl_to_tree(
      s"""
      let bool_elim: (
        C: (bool_t) -> type_t,
        C(true),
        C(false),
        b: bool_t,
      ) -> C(b) = C => h0 => h1 => {
        case true[] => h0
        case false[] => h1
      }
      """,
      DeclLet("bool_elim",
        pi("C" :: "bool_t" ->: Univ()) {
          ("C" $ "true") ->:
          ("C" $ "false") ->:
          pi("b" :: "bool_t") { "C" $ "b" } },
        fn("C", "h0", "h1") { mat(
          "true" -> fn(__) { "h0" },
          "false" -> fn(__) { "h1" }) })
    )

    assert_decl_to_tree(
      s"""
      let bool_elim: (
        C: (bool_t) -> type_t,
        C(true[]),
        C(false[]),
        b: bool_t,
      ) -> C(b) = (C, h0, h1) => {
        case true[] => h0
        case false[] => h1
      }
      """,
      DeclLet("bool_elim",
        pi("C" :: "bool_t" ->: Univ()) {
          ("C" $ %("true")) ->:
          ("C" $ %("false")) ->:
          pi("b" :: "bool_t") { "C" $ "b" } },
        fn("C", "h0", "h1") { mat(
          "true" -> fn(__) { "h0" },
          "false" -> fn(__) { "h1" }) })
    )

    assert_decl_to_tree(
      s"""
      letrec nat_t: type_t = type {
        case zero
        case succ[nat_t]
      }
      """,
      DeclLetrec("nat_t", Univ(),
        sum(
          "zero" -> Trivial(),
          "succ" -> "nat_t"))
    )

    assert_decl_to_tree(
      s"""
      let zero: nat_t = zero[]
      """,
      DeclLet("zero", "nat_t", %("zero"))
    )


    assert_decl_to_tree(
      s"""
      let zero: nat_t = zero[]
      """,
      DeclLet("zero", "nat_t", %("zero"))
    )
  }

}
