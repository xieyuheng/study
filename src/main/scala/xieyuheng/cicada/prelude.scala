package xieyuheng.cicada

import xieyuheng.cicada.dsl._

object prelude {

  val nat = Env()

  .defType("Nat", $(),
    members = $(
      "Zero" -> $(),
      "Succ" -> $("prev" -> "Nat")))

  val list = Env()

  .defType("List", $("A" -> Type()),
    members = $(
      "Null" -> $("A" -> Type()),
      "Cons" -> $(
        "A" -> Type(),
        "head" -> The("A"),
        "tail" -> The("List" ap $("A" -> "A")))))

  .defExp("cdr", Fn(
    args = $(
      "list" -> The("List")),
    ret = The("List"),
    body = "list" dot "tail"))

  .defFn("list_append",
    args = $(
      "ante" -> The("List"),
      "succ" -> The("List")),
    ret = The("List"),
    body = Case("ante", $(
      "Null" -> "succ",
      "Cons" -> ("Cons" ap $(
        "A" -> ("ante" dot "A"),
        "head" -> ("ante" dot "head"),
        "tail" -> ("list_append" ap $(
          "ante" -> ("ante" dot "tail"),
          "succ" -> "succ")))))))

  val vec = Env()

  .importAll(nat)

  .defType("Vec", $(
    "A" -> Type(),
    "length" -> The("Nat")),
    members = $(
      "NullVec" -> $(
        "A" -> Type(),
        "length" -> The("Nat"),
        "length" -> "Zero"),
      "ConsVec" -> $(
        "A" -> Type(),
        "length" -> The("Nat"),
        "n" -> The("Nat"),
        "length" -> ("Succ" ap $("prev" -> "n")),
        "head" -> The("A"),
        "tail" -> The("Vec" ap $("A" -> "A", "length" -> "n")))))

}
