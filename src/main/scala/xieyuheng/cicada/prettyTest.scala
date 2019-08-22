package xieyuheng.cicada

import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._

object prettyTest extends Test {
  val `pretty can make Exp pretty` = {

    println(s"------")

    val t = prettyExp(Type())

    println(s"t: ${t}")

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

    println(s"threeZeros: ${threeZeros}")

    val fn = prettyExp(
      Fn(
        args = $(
          "x" -> "nat_t",
          "y" -> "nat_t"),
        ret = "nat_t",
        body = "zero_t"))

    println(s"fn: ${fn}")

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

    println(s"nestedFn: ${nestedFn}")
    println(s"------")
  }
}
