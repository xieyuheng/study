package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object paper_without_parser extends Module {

  let("id",
    pi("A" :: Univ()) { "A" ->: "A" },
    fn("A", "x") { "x" })

  let("bool_t", Univ(),
    sum(
      "true" -> Trivial(),
      "false" -> Trivial()))

  let("true", "bool_t", %("true"))
  let("false", "bool_t", %("false"))

  let("bool_elim",
    pi("C" :: "bool_t" ->: Univ()) {
      ("C" $ %("true")) ->:
      ("C" $ %("false")) ->:
      pi("b" :: "bool_t") { "C" $ "b" } },
    fn("C", "h0", "h1") { mat(
      "true" -> fn("_") { "h0" },
      "false" -> fn("_") { "h1" }) })

  letrec("nat_t", Univ(),
    sum(
      "zero" -> Trivial(),
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

  letrec("nat_rec",
    pi("C" :: "nat_t" ->: Univ()) {
      ("C" $ %("zero")) ->:
      pi("n" :: "nat_t") { ("C" $ "n") ->: ("C" $ %("succ", "n")) } ->:
      pi("n" :: "nat_t") { ("C" $ "n") } },
    fn("C", "a", "g") { mat(
      "zero" -> fn("_") { "a" },
      "succ" -> fn("prev") { "g" $ "prev" $ ("nat_rec" $ "C" $ "a" $ "g" $ "prev") }) })

  letrec("add", "nat_t" ->: "nat_t" ->: "nat_t",
    mat(
      "zero" -> fn("_") { fn("y") { "y" } },
      "succ" -> fn("prev") { fn("y") { %("succ", "add" $ "prev" $ "y") } }))

  let("double", "nat_t" ->: "nat_t",
    fn("x") { "add" $ "x" $ "x" })

  letrec("mul", "nat_t" ->: "nat_t" ->: "nat_t",
    mat(
      "zero" -> fn("_") { fn("y") { %("zero") } },
      "succ" -> fn("prev") { fn("y") { "add" $ "y" $ ("mul" $ "prev" $ "y") } }) )

  let("square", "nat_t" ->: "nat_t",
    fn("x") { "mul" $ "x" $ "x" })

  letrec("nat_eq", "nat_t" ->: "nat_t" ->: "bool_t",
    mat(
      "zero" -> fn("_") { mat(
        "zero" -> fn("_") { %("true") },
        "succ" -> fn("_") { %("false") }) },
      "succ" -> fn("x") { mat(
        "zero" -> fn("_") { %("false") },
        "succ" -> fn("y") { "nat_eq" $ "x" $ "y" }) }))

  letrec("list_t", Univ() ->: Univ(),
    fn("A") { sum(
      "nil" -> Trivial(),
      "cons" -> "A" * ("list_t" $ "A")) })

  letrec("list_append",
    pi("A" :: Univ()) {
      "list_t" $ "A" ->: "list_t" $ "A" ->: "list_t" $ "A" },
    fn("A") { mat(
      "nil" -> fn("_") { fn("y") { "y" } },
      "cons" -> fn("car" * "cdr") { fn("y") {
        %("cons", "car" * ("list_append" $ "A" $ "cdr" $ "y")) } }) } )

}
