package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._

object vec extends Module {

  import_all(nat)

  // define_type("vec_t", %(
  //   "A" -> the_type,
  //   "length" -> the("nat_t")),
  //   members = %(
  //     "null_vec_t" -> %(
  //       "length" -> "zero_t"),
  //     "cons_vec_t" -> %(
  //       "n" -> the("nat_t"),
  //       "length" -> ("succ_t" ap %("prev" -> "n")),
  //       "head" -> the("A"),
  //       "tail" -> the("vec_t" ap %("A" -> "A", "length" -> "n")))))


  define_type("vec_t", %(
    "A" :: Type(),
    "length" :: "nat_t"),
    members = %(
      "null_vec_t" -> %(
        "length" := "zero_t"),
      "cons_vec_t" -> %(
        "n" :: "nat_t",
        "length" := ("succ_t" ap %("prev" -> "n")),
        "head" :: "A",
        "tail" :: ("vec_t" ap %("A" -> "A", "length" -> "n")))))

  // TODO
  // vec_map
  // vec_append
}
