package xieyuheng.mini_tt

import xieyuheng.mini_tt.expDSL._

object paper extends Module {

  s"""
  id(A: U, x: A): A = x
  """

  s"""
  id: (A: U) -> A = (x) => x
  """

  let("id",
    pi("A" :: U) { "A" ->: "A" },
    fn("A", "x") { "x" })

  s"""
  bool_t: U = sum {
    true
    false
  }
  """

  let("bool_t", U,
    sum(
      "true" -> Trivial,
      "false" -> Trivial))

  let("true", "bool_t", %("true"))
  let("false", "bool_t", %("false"))

  s"""
  bool_elim : (C : bool_t -> U) ->
              C true ->
              C false ->
              (b : bool_t) -> C b
  bool_elim C h0 h1 = choice {
    true => h0
    false => h1
  }
  """

  s"""
  bool_elim(
    C: bool_t -> U,
    h0: C(true),
    h1: C(false),
  ): (b : bool_t) -> C(b) = choice {
    true => h0
    false => h1
  }
  """

  s"""
  bool_elim:
    (C: bool_t -> U) ->
    C(true) ->
    C(false) ->
    (b : bool_t) -> C(b) =
  (C, h0, h1) => choice {
    true => h0
    false => h1
  }
  """

  let("bool_elim",
    pi("C" :: "bool_t" ->: U) {
      ("C" $ %("true")) ->:
      ("C" $ %("false")) ->:
      pi("b" :: "bool_t") { "C" $ "b" } },
    fn("C", "h0", "h1") { choice(
      "true" -> fn("_") { "h0" },
      "false" -> fn("_") { "h1" }) })

  s"""
  nat_t: U = sum {
    zero
    succ nat_t
  }
  """

  letrec("nat_t", U,
    sum(
      "zero" -> Trivial,
      "succ" -> "nat_t"))

  let("zero", "nat_t", %("zero"))
  let("one", "nat_t", %("succ", "zero"))
  let("two", "nat_t", %("succ", "one"))
  let("three", "nat_t", %("succ", "two"))
  let("four", "nat_t", %("succ", "three"))
  let("five", "nat_t", %("succ", "four"))
  let("six", "nat_t", %("succ", "five"))
  let("seven", "nat_t", %("succ", "six"))
  let("eight", "nat_t", %("succ", "seven"))
  let("nine", "nat_t", %("succ", "eight"))
  let("ten", "nat_t", %("succ", "nine"))


  s"""
  nat_rec : (C : nat_t -> U) ->
            C zero ->
            ((n : nat_t) -> C n -> C (succ n)) ->
            ((n : nat_t) -> C n)
  nat_rec C a g = choice {
    zero => a
    succ prev => g prev (nat_rec C a g prev)
  }
  """

  s"""
  nat_rec(
    C: nat_t -> U,
    a: C(zero),
    g: (n: nat_t, C(n)) -> C(succ(n)),
  ): (n: nat_t) -> C(n) = choice {
    zero => a
    succ(prev) => g(prev, nat_rec(C, a, g, prev))
  }
  """

  letrec("nat_rec",
    pi("C" :: "nat_t" ->: U) {
      ("C" $ %("zero")) ->:
      pi("n" :: "nat_t") { ("C" $ "n") ->: ("C" $ %("succ", "n")) } ->:
      pi("n" :: "nat_t") { ("C" $ "n") } },
    fn("C", "a", "g") { choice(
      "zero" -> fn("_") { "a" },
      "succ" -> fn("prev") { "g" $ "prev" $ ("nat_rec" $ "C" $ "a" $ "g" $ "prev") }) })

  s"""
  add(x: nat_t): (y: nat_t) -> nat_t = choice {
    zero => (y: nat_t): nat_t => y
    succ prev => (y: nat_t): nat_t => succ(add(prev, y))
  }
  """

  letrec("add", "nat_t" ->: "nat_t" ->: "nat_t",
    choice(
      "zero" -> fn("_") { fn("y") { "y" } },
      "succ" -> fn("prev") { fn("y") { %("succ", "add" $ "prev" $ "y") } }))

  let("double", "nat_t" ->: "nat_t",
    fn("x") { "add" $ "x" $ "x" })

  letrec("mul", "nat_t" ->: "nat_t" ->: "nat_t",
    choice(
      "zero" -> fn("_") { fn("y") { %("zero") } },
      "succ" -> fn("prev") { fn("y") { "add" $ "y" $ ("mul" $ "prev" $ "y") } }) )

  let("square", "nat_t" ->: "nat_t",
    fn("x") { "mul" $ "x" $ "x" })

  s"""
  nat_eq : nat_t -> nat_t -> bool_t
  nat_eq = choice {
    zero => choice {
      zero => true
      succ _ => false
    }
    succ x => choice {
      zero => false
      succ y => nat_eq x y
    }
  }
  """

  s"""
  nat_eq : nat_t -> nat_t -> bool_t = choice {
    zero => choice {
      zero => true
      succ => false
    }
    succ[x] => choice {
      zero => false
      succ[y] => nat_eq(x, y)
    }
  }
  """

  s"""
  nat_eq(x: nat_t, y: nat_t): bool_t =
  x match {
    zero => y match {
      zero => true
      succ => false
    }
    succ(x_prev) => y match {
      zero => false
      succ(y_prev) => nat_eq(x_prev, y_prev)
    }
  }
  """

  s"""
  nat_eq : nat_t -> nat_t -> bool_t = match {
    zero, zero => true
    zero, succ => false
    succ(x), zero => false
    succ(x), succ(y) => nat_eq(x, y)
  }
  """

  letrec("nat_eq", "nat_t" ->: "nat_t" ->: "bool_t",
    choice(
      "zero" -> fn("_") { choice(
        "zero" -> fn("_") { %("true") },
        "succ" -> fn("_") { %("false") }) },
      "succ" -> fn("x") { choice(
        "zero" -> fn("_") { %("false") },
        "succ" -> fn("y") { "nat_eq" $ "x" $ "y" }) }))

  s"""
  list_t: U -> U = (A) => sum {
    nil()
    cons(A, list_t(A))
  }
  """

  letrec("list_t", U ->: U,
    fn("A") { sum(
      "nil" -> Trivial,
      "cons" -> "A" * ("list_t" $ "A")) })

  letrec("list_append",
    pi("A" :: U) {
      "list_t" $ "A" ->: "list_t" $ "A" ->: "list_t" $ "A" },
    fn("A") { choice(
      "nil" -> fn("_") { fn("y") { "y" } },
      "cons" -> fn("car" * "cdr") { fn("y") {
        %("cons", "car" * ("list_append" $ "A" $ "cdr" $ "y")) } }) } )

}
