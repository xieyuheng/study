import org.scalatest._
import xieyuheng.cicada._
import xieyuheng.cicada.pretty._

class prettySpec extends FlatSpec with Matchers {
  "pretty" can "make Exp pretty" in {

    val t = prettyExp(Type())

    val threeZeros = prettyExp(
      Ap(Var("cons_t"), MultiMap(
        "A" -> Var("nat_t"),
        "head" -> Var("zero_t"),
        "tail" -> Ap(Var("cons_t"), MultiMap(
          "A" -> Var("nat_t"),
          "head" -> Var("zero_t"),
          "tail" -> Ap(Var("cons_t"), MultiMap(
            "A" -> Var("nat_t"),
            "head" -> Var("zero_t"),
            "tail" -> Var("null_t"))))))))

    val fn = prettyExp(
      Fn(
        args = MultiMap(
          "x" -> Var("nat_t"),
          "y" -> Var("nat_t")),
        ret = Var("nat_t"),
        body = Var("zero_t")))

    val nestedFn = prettyExp(
      Fn(
        args = MultiMap(
          "x" -> Var("nat_t"),
          "y" -> Var("nat_t")),
        ret = Pi(MultiMap(
          "x" -> Var("nat_t"),
          "y" -> Var("nat_t")),
          Var("nat_t")),
        body = Fn(MultiMap(
          "x" -> Var("nat_t"),
          "y" -> Var("nat_t")),
          Var("nat_t"),
          Var("zero_t"))))

    println(s"------")
    println(s"t: ${t}")
    println(s"threeZeros: ${threeZeros}")
    println(s"fn: ${fn}")
    println(s"nestedFn: ${nestedFn}")
    println(s"------")
  }
}
