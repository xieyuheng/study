import org.scalatest._
import xieyuheng.cicada._

class PrettySpec extends FlatSpec with Matchers {
  "Pretty" can "make Exp pretty" in {

    val t = Pretty.Exp(Type(), 0)

    val threeZeros = Pretty.Exp(
      Ap(Var("cons_t"), MultiMap(
        "A" -> Var("nat_t"),
        "head" -> Var("zero_t"),
        "tail" -> Ap(Var("cons_t"), MultiMap(
          "A" -> Var("nat_t"),
          "head" -> Var("zero_t"),
          "tail" -> Ap(Var("cons_t"), MultiMap(
            "A" -> Var("nat_t"),
            "head" -> Var("zero_t"),
            "tail" -> Var("null_t"))))))),
      0)

    val fn = Pretty.Exp(
      Fn(
        args = MultiMap(
          "x" -> Var("nat_t"),
          "y" -> Var("nat_t")),
        ret = Var("nat_t"),
        body = Var("zero_t")),
      0)

    val nestedFn = Pretty.Exp(
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
          Var("zero_t"))),
      0)

    println(s"------")
    println(s"t: ${t}")
    println(s"threeZeros: ${threeZeros}")
    println(s"fn: ${fn}")
    println(s"nestedFn: ${nestedFn}")
    println(s"------")
  }
}
