import org.scalatest._
import xieyuheng.cicada._

class evalSpec extends FlatSpec with Matchers {
  "eval" should "eval Type to LogicVar" in {
    val env = Env()
    for {
      t <- eval(Type(), env)
    } println(t)
  }

  it should "eval undefined Var to NeutralValue" in {
    val env = Env()
    for {
      neu <- eval(Var("x"), env)
    } assert(neu == NeutralValue(VarNeutral("x")))
  }

  it should "eval defined Var to value" in {
    val env = Env()
      .defineValue("x", LogicVar("#x"))
      .defineValue("y", LogicVar("#y"))

    for {
      x <- eval(Var("x"), env)
      y <- eval(Var("y"), env)
    } {
      assert(x == LogicVar("#x"))
      assert(y == LogicVar("#y"))
    }
  }

  val NatModule = Env()
    .defineUnion("Nat", MultiMap(), List("Zero", "Succ"))
    .defineRecord("Zero", MultiMap())
    .defineRecord("Succ", MultiMap("prev" -> Var("Nat")))

//   it should "eval NatModule" in {
//     val module = NatModule

//     for {
//       nat <- eval(Var("Nat"), module)
//       zero <- eval(Var("Zero"), module)
//       succ <- eval(Var("Succ"), module)
//       one <- eval(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), module)
//       z1 <- eval(Field(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), "prev"), module)
//     } {
//       println(s"${Pretty.fromValue(nat, 0)}")
//       println(s"${Pretty.fromValue(zero, 0)}")
//       println(s"${Pretty.fromValue(succ, 0)}")
//       println(s"one -- ${Pretty.fromValue(one, 0)}")
//       println(s"z1 -- ${Pretty.fromValue(z1, 0)}")
//     }
//   }

  val ListModule = Env()
    .defineUnion("List", MultiMap("A" -> Type()), List("Null", "Cons"))
    .defineRecord("Null", MultiMap("A" -> Type()))
    .defineRecord("Cons", MultiMap(
      "A" -> Type(),
      "head" -> Var("A"),
      "tail" -> Ap(Var("List"), MultiMap("A" -> Var("A"))),
    ))

  def pp(exp: Exp, env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${Pretty.fromValue(value, 0)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }

  it should "eval ListModule" in {
    val module = ListModule.importAll(NatModule)

    pp(Var("List"), module)
    pp(Var("Null"), module)
    pp(Var("Cons"), module)

    pp(Var("Nat"), module)
    pp(Var("Zero"), module)
    pp(Var("Succ"), module)

    pp(
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
      module)

    pp(
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> Var("Zero"),
        "tail" -> Var("Null"))),
      module)
  }

//   "unify" should "not make bind weaker" in {
//     val bind = Map(
//       "2bda84b0-dd85-43c9-a94c-c00df82c9f9f" ->
//         UnionValue(
//           "fa21c03d-9b7d-48ba-9bf9-9d0bfe088eba",
//           "Nat",
//           MultiMap(List()),
//           List("Zero", "Succ"),
//           Map()))

//     val srcValue = RecordValue("Zero", MultiMap(List()), Map())

//     val tarValue = LogicVar("2bda84b0-dd85-43c9-a94c-c00df82c9f9f")

//     println(s"before -- ${bind}")
//     println(s"--------- ${srcValue} unify ${tarValue}")
//     println(s"after --- ${unify(srcValue, tarValue, bind)}")
//     println()

//     println(s"util.walk(srcValue, bind) -- ${util.walk(srcValue, bind)}")
//     println(s"util.walk(tarValue, bind) -- ${util.walk(tarValue, bind)}")

//   }
}
