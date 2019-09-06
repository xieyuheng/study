package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._

object vec_test extends Module with App {

  import_all(vec)

  eval_print("vec_t")
  eval_print("null_vec_t")
  eval_print("cons_vec_t")
}
