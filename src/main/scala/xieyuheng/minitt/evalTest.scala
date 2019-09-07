package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object evalTest extends Module with App {

  import_all(paper)

  println(run(
    "id" $ "Bool" $ %("true")))

  println(run(
    "add" $
      %("succ", %("zero")) $
      %("succ", %("zero"))))

  println(run(
    %("cons", %("zero") * %("nil"))))

  println(run(
    %("nil")))

  println(run(
    "eqNat" $
      %("succ", %("succ", %("zero"))) $
      %("succ", %("succ", %("zero")))))

  println(run(
    "eqNat" $
      %("succ", %("zero")) $
      %("succ", %("succ", %("zero")))))
}
