package cicada.prelude

import cicada._
import cicada.dsl._
import cicada.pretty._

object list {

  val env = Env()

  .defType("list_t", $("A" -> Type()),
    members = $(
      "null_t" -> $(),
      "cons_t" -> $(
        "head" -> The("A"),
        "tail" -> The("list_t" ap $("A" -> "A")))))

  .defExp("cdr", Fn(
    args = $(
      "list" -> The("list_t")),
    ret = The("list_t"),
    body = "list" dot "tail"))

  .defFn("list_append",
    args = $(
      "ante" -> The("list_t"),
      "succ" -> The("list_t")),
    ret = The("list_t"),
    body = Case("ante", $(
      "null_t" -> "succ",
      "cons_t" -> ("cons_t" ap $(
        "A" -> ("ante" dot "A"),
        "head" -> ("ante" dot "head"),
        "tail" -> ("list_append" ap $(
          "ante" -> ("ante" dot "tail"),
          "succ" -> "succ")))))))

}
