package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._

object vec_test extends App {
  implicit val module = vec.env

  util.evalPrint("vec_t")
  util.evalPrint("null_vec_t")
  util.evalPrint("cons_vec_t")
}
