import org.scalatest._
import xieyuheng.cicada._

class PrettySpec extends FlatSpec with Matchers {
  "Pretty" can "make Exp pretty" in {

    val t = Pretty.fromExp(Type(), 0)

    val threeZeros = Pretty.fromExp(
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> Var("Zero"),
        "tail" -> Ap(Var("Cons"), MultiMap(
          "A" -> Var("Nat"),
          "head" -> Var("Zero"),
          "tail" -> Ap(Var("Cons"), MultiMap(
            "A" -> Var("Nat"),
            "head" -> Var("Zero"),
            "tail" -> Var("Null"))))))),
      0)

    val fn = Pretty.fromExp(
      Fn(
        args = MultiMap(
          "x" -> Var("Nat"),
          "y" -> Var("Nat")),
        ret = Var("Nat"),
        body = Var("Zero")),
      0)

    val nestedFn = Pretty.fromExp(
      Fn(
        args = MultiMap(
          "x" -> Var("Nat"),
          "y" -> Var("Nat")),
        ret = Pi(MultiMap(
          "x" -> Var("Nat"),
          "y" -> Var("Nat")),
          Var("Nat")),
        body = Fn(MultiMap(
          "x" -> Var("Nat"),
          "y" -> Var("Nat")),
          Var("Nat"),
          Var("Zero"))),
      0)

    println(s"------")
    println(s"t: ${t}")
    println(s"threeZeros: ${threeZeros}")
    println(s"fn: ${fn}")
    println(s"nestedFn: ${nestedFn}")
    println(s"------")
  }

  it can "make Value pretty" in {

  }
}
