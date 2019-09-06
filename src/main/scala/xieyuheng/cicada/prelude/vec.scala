package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._

object vec {

  val env = Env()

  .import_all(nat.env)

  .define_type("vec_t", %(
    "A" -> the_type,
    "length" -> the("nat_t")),
    members = %(
      "null_vec_t" -> %(
        "length" -> "zero_t"),
      "cons_vec_t" -> %(
        "n" -> the("nat_t"),
        "length" -> ("succ_t" ap %("prev" -> "n")),
        "head" -> the("A"),
        "tail" -> the("vec_t" ap %("A" -> "A", "length" -> "n")))))

}
