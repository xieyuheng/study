package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object paper extends Module {

  // id : (A : U) -> A -> A
  // id A, x = x

  let("id",
    pi("A" :: U) { "A" ->: "A" },
    fn("A", "x") { "x" })

  // bool_t : U
  // bool_t = sum {
  //   true
  //   false
  // }

  let("bool_t", U,
    sum(
      "true" -> Trivial,
      "false" -> Trivial))

  // bool_elim : (C : bool_t -> U) ->
  //             C true ->
  //             C false ->
  //             (b : bool_t) -> C b
  // bool_elim C h0 h1 = choice {
  //   true => h0
  //   false => h1
  // }

  let("bool_elim",
    pi("C" :: "bool_t" ->: U) {
      ("C" $ %("true")) ->:
      ("C" $ %("false")) ->:
      pi("b" :: "bool_t") { "C" $ "b" } },
    fn("C", "h0", "h1") { choice(
      "true" -> fn("_") { "h0" },
      "false" -> fn("_") { "h1" }) })

  // nat_t : U
  // nat_t = sum {
  //   zero
  //   succ nat_t
  // }

  letrec("nat_t", U,
    sum(
      "zero" -> Trivial,
      "succ" -> "nat_t"))

  // list_t : U -> U
  // list_t A = sum {
  //   nil
  //   cons A list_t A
  // }

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

  // nat_rec : (C : nat_t -> U) ->
  //          C zero ->
  //          ((n : nat_t) -> C n -> C (succ n)) ->
  //          ((n : nat_t) -> C n)
  // nat_rec C a g = choice {
  //   zero => a
  //   succ prev => g prev (nat_rec C a g prev)
  // }

  letrec("nat_rec",
    pi("C" :: "nat_t" ->: U) {
      ("C" $ %("zero")) ->:
      pi("n" :: "nat_t") { ("C" $ "n") ->: ("C" $ %("succ", "n")) } ->:
      pi("n" :: "nat_t") { ("C" $ "n") } },
    fn("C", "a", "g") { choice(
      "zero" -> fn("_") { "a" },
      "succ" -> fn("prev") { "g" $ "prev" $ ("nat_rec" $ "C" $ "a" $ "g" $ "prev") }) })

  // add : nat_t -> nat_t -> nat_t
  // add x = choice {
  //   zero => x
  //   succ prev => succ (add x prev)
  // }

  letrec("add", "nat_t" ->: "nat_t" ->: "nat_t",
    fn("x") { choice(
      "zero" -> fn("_") { "x" },
      "succ" -> fn("prev") { %("succ", "add" $ "x" $ "prev") }) })

  // nat_eq : nat_t -> nat_t -> bool_t
  // nat_eq = choice {
  //   zero => choice {
  //     zero => true
  //     succ _ => false
  //   }
  //   succ x => choice {
  //     zero => false
  //     succ y => nat_eq x y
  //   }
  // }

  letrec("nat_eq", "nat_t" ->: "nat_t" ->: "bool_t",
    choice(
      "zero" -> fn("_") { choice(
        "zero" -> fn("_") { %("true") },
        "succ" -> fn("_") { %("false") }) },
      "succ" -> fn("x") { choice(
        "zero" -> fn("_") { %("false") },
        "succ" -> fn("y") { "nat_eq" $ "x" $ "y" }) }))

}
