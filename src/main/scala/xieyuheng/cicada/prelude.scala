package xieyuheng.cicada

import xieyuheng.cicada.dsl._

object prelude {

  val nat = Env()

  .defType("nat_t", $(),
    members = $(
      "zero_t" -> $(),
      "succ_t" -> $("prev" -> "nat_t")))

  val list = Env()

  .defType("list_t", $("A" -> Type()),
    members = $(
      "null_t" -> $("A" -> Type()),
      "cons_t" -> $(
        "A" -> Type(),
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

  val vec = Env()

  .importAll(nat)

  .importAll(nat)

  .defType("vec_t", $(
    "A" -> Type(),
    "length" -> The("nat_t")),
    members = $(
      "null_vec_t" -> $(
        "A" -> Type(),
        "length" -> The("nat_t"),
        "length" -> "zero_t"),
      "cons_vec_t" -> $(
        "A" -> Type(),
        "length" -> The("nat_t"),
        "n" -> The("nat_t"),
        "length" -> ("succ_t" ap $("prev" -> "n")),
        "head" -> The("A"),
        "tail" -> The("vec_t" ap $("A" -> "A", "length" -> "n")))))

}
