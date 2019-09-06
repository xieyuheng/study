package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._

object bool extends Module {

  define_type("bool_t", %(),
    members = %(
      "true_t" -> %(),
      "false_t" -> %()))

  define("true", "true_t")
  define("false", "false_t")

}
