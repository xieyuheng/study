import org.scalatest._

import xieyuheng.cicada._
import xieyuheng.cicada.dsl._

class preludeListSpec extends FlatSpec with Matchers {
  "prelude.list" should "work" in {
    implicit val module = prelude.list.importAll(prelude.nat)

    ep("list_t")
    ep("null_t")
    ep("cons_t")

    ep("nat_t")
    ep("zero_t")
    ep("succ_t")

    val zero: Exp = "zero_t"

    val threeZeros =
      "cons_t" ap $(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> zero,
          "tail" -> ("cons_t" ap $(
            "A" -> "nat_t",
            "head" -> zero,
            "tail" -> "null_t")))))

    ep(threeZeros)

    val one = "succ_t" ap $("prev" -> zero)

    val zeroAndOne =
      "cons_t" ap $(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> one,
          "tail" -> "null_t")))

    ep(zeroAndOne)

    ep("cons_t" ap $(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> "null_t"))

    ep("cons_t" ap $(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> ("null_t" ap $("A" -> "nat_t"))))

    val twoZeros = "cdr" ap $(
      "list" -> threeZeros)

    val oneZero = "cdr" ap $(
      "list" -> twoZeros)

    ep(twoZeros)
    ep(oneZero)

    ep("list_append" ap $(
      "ante" -> threeZeros,
      "succ" -> threeZeros))
  }
}
