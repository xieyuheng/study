package cicada

import cicada.dsl._
import cicada.pretty._

object prettyTest extends Test {
  val `pretty can make Exp pretty` = {
    val t = prettyExp(Type())

    val threeZeros = prettyExp(
      "cons_t" ap $(
        "A" -> "nat_t",
        "head" -> "zero_t",
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> "zero_t",
          "tail" -> ("cons_t" ap $(
            "A" -> "nat_t",
            "head" -> "zero_t",
            "tail" -> "null_t"))))))

    val fn = prettyExp(
      Fn(
        args = $(
          "x" -> "nat_t",
          "y" -> "nat_t"),
        ret = "nat_t",
        body = "zero_t"))

    val nestedFn = prettyExp(
      Fn(
        args = $(
          "x" -> "nat_t",
          "y" -> "nat_t"),
        ret = Pi(
          args = $(
            "x" -> "nat_t",
            "y" -> "nat_t"),
          ret = "nat_t"),
        body = Fn(
          args = $(
            "x" -> "nat_t",
            "y" -> "nat_t"),
          ret = "nat_t",
          body = "zero_t")))

    println(s"------")
    println(s"t: ${t}")
    println(s"threeZeros: ${threeZeros}")
    println(s"fn: ${fn}")
    println(s"nestedFn: ${nestedFn}")
    println(s"------")
  }
}
