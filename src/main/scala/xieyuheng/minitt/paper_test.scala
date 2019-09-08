package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object paper_test extends Module with App {

  import_all(paper)

  run_print(
    "id" $ "bool_t" $ %("true"))

  run_print(
    "add" $
      %("succ", %("zero")) $
      %("succ", %("zero")))

  run_print(
    %("cons", %("zero") * %("nil")))

  run_print(
    %("nil"))

  run_print(
    "nat_eq" $
      %("succ", %("succ", %("zero"))) $
      %("succ", %("succ", %("zero"))))

  run_print(
    "nat_eq" $
      %("succ", %("zero")) $
      %("succ", %("succ", %("zero"))))

  println("------\n")

  run_print(
    "list_append" $ "nat_t")

  run_print(
    "list_append" $ "nat_t" $ %("nil"))

  run_print(
    "list_append" $ "nat_t" $ %("nil") $ %("nil"))

  println("------\n")

  run_print(
    "list_append" $ "nat_t")

  run_print(
    "list_append" $ "nat_t" $ %("cons", %("zero") * %("nil")))

  run_print(
    "list_append" $ "nat_t" $ %("cons", %("zero") * %("nil")) $ %("nil"))

  println("------\n")

  val two_zeros = %("cons", %("zero") * %("cons", %("zero") * %("nil")))

  run_print(
    "list_append" $ "nat_t" $ two_zeros $ two_zeros)

}
