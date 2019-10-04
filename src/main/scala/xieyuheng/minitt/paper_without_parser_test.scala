package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object paper_without_parser_test extends Module with App {

  import_all(paper_without_parser)

  assert_eq("id" $ "bool_t" $ "true", "true")
  assert_eq("id" $ "bool_t" $ "false", "false")
  assert_eq("id" $ "bool_t" $ "id", "id")

  assert_eq("double" $ "one", "two")
  assert_eq("double" $ "two", "four")
  assert_eq("double" $ "three", "six")

  assert_eq("square" $ "one", "one")
  assert_eq("square" $ "two", "four")
  assert_eq("square" $ "three", "nine")

  assert_eq("nat_eq" $ "one" $ "one", "true")
  assert_eq("nat_eq" $ "two" $ "two", "true")
  assert_eq("nat_eq" $ "one" $ "two", "false")
  assert_eq("nat_eq" $ "two" $ "one", "false")

  println("------\n")

  show(
    "list_append" $ "nat_t")

  show(
    "list_append" $ "nat_t" $ %("nil"))

  show(
    "list_append" $ "nat_t" $ %("nil") $ %("nil"))

  println("------\n")

  show(
    "list_append" $ "nat_t")

  show(
    "list_append" $ "nat_t" $ %("cons", %("zero") * %("nil")))

  show(
    "list_append" $ "nat_t" $ %("cons", %("zero") * %("nil")) $ %("nil"))

  println("------\n")

  val two_zeros = %("cons", %("zero") * %("cons", %("zero") * %("nil")))

  show(
    "list_append" $ "nat_t" $ two_zeros $ two_zeros)

}
