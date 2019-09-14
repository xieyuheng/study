package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._
import xieyuheng.cicada.with_logic_variable.pretty._

object vec_test extends Module with App {

  import_all(vec)

  eval_print("vec_t")
  eval_print("null_vec_t")
  eval_print("cons_vec_t")
}
