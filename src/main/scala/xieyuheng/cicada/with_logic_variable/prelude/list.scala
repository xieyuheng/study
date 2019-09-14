package xieyuheng.cicada.with_logic_variable.prelude

import xieyuheng.cicada.with_logic_variable._
import xieyuheng.cicada.with_logic_variable.expDSL._

object list extends Module {

  import_all(nat)

  define_type("list_t", %("A" -> the_type),
    members = %(
      "null_t" -> %(),
      "cons_t" -> %(
        "head" -> the("A"),
        "tail" -> the("list_t" ap %("A" -> "A")))))

  define_fn("cdr",
    args = %("list" -> the("list_t")),
    ret = the("list_t"),
    body = "list" dot "tail")

  define_fn("list_length",
    args = %("list" -> the("list_t")),
    ret = the("nat_t"),
    body = choice("list", %(
      "null_t" -> "zero_t",
      "cons_t" -> ("succ_t" ap %(
        "prev" -> ("list_length" ap %(
          "list" -> ("list" dot "tail"))))))))

  define_fn("list_append",
    args = %(
      "ante" -> the("list_t"),
      "succ" -> the("list_t")),
    ret = the("list_t"),
    body = choice("ante", %(
      "null_t" -> "succ",
      "cons_t" -> ("cons_t" ap %(
        "A" -> ("ante" dot "A"),
        "head" -> ("ante" dot "head"),
        "tail" -> ("list_append" ap %(
          "ante" -> ("ante" dot "tail"),
          "succ" -> "succ")))))))

  define_fn("list_map",
    args = %(
      "A" :: Type(),
      "B" :: Type(),
      "f" :: pi(%("x" :: "A"), the("B")),
      "list" :: ("list_t" ap %("A" := "A"))),
    ret = the("list_t" ap %("A" := "B")),
    body = choice("list", %(
      "null_t" := "null_t",
      "cons_t" := ("cons_t" ap %(
        "A" := "B",
        "head" := ("f" ap %("x" -> ("list" dot "head"))),
        "tail" := ("list_map" ap %(
          "A" := "A",
          "B" := "B",
          "f" := "f",
          "list" := ("list" dot "tail"))))))))

  // define_fn("list_map",
  //   args = %(
  //     "A" -> the_type,
  //     "B" -> the_type,
  //     "f" -> the(pi(%("x" -> the("A")), the("B"))),
  //     "list" -> the("list_t" ap %("A" -> "A"))),
  //   ret = the("list_t" ap %("A" -> "B")),
  //   body = choice("list", %(
  //     "null_t" -> "null_t",
  //     "cons_t" -> ("cons_t" ap %(
  //       "A" -> "B",
  //       "head" -> ("f" ap %("x" -> ("list" dot "head"))),
  //       "tail" -> ("list_map" ap %(
  //         "A" -> "A",
  //         "B" -> "B",
  //         "f" -> "f",
  //         "list" -> ("list" dot "tail"))))))))

}
