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

  it should "eval NatModule" in {
    val module = NatModule

    for {
      nat <- eval(Var("Nat"), module)
      zero <- eval(Var("Zero"), module)
      succ <- eval(Var("Succ"), module)
      one <- eval(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), module)
      z1 <- eval(Field(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), "prev"), module)
    } {
      println(s"Nat: ${nat}")
      println(s"Zero: ${zero}")
      println(s"Succ: ${succ}")
      println(s"one: ${one}")
      println(s"z1: ${z1}")
    }
  }

  val ListModule = Env()
    .defineUnion("List", MultiMap("A" -> Type()), List("Null", "Cons"))
    .defineRecord("Null", MultiMap("A" -> Type()))
    .defineRecord("Cons", MultiMap(
      "A" -> Type(),
      "head" -> Var("A"),
      "tail" -> Ap(Var("List"), MultiMap("A" -> Var("A"))),
    ))

  it should "eval ListModule" in {
    val module = ListModule.importAll(NatModule)

    for {
      list <- eval(Var("List"), module)
      null$ <- eval(Var("Null"), module)
      cons <- eval(Var("Cons"), module)

      nat <- eval(Var("Nat"), module)
      zero <- eval(Var("Zero"), module)
      succ <- eval(Var("Succ"), module)

      threeZeros <- eval(
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

    } {
//       println(s"List: ${list}")
//       println(s"Null: ${null$}")
//       println(s"Cons: ${cons}")

//       println(s"Nat: ${nat}")
//       println(s"Zero: ${zero}")
//       println(s"Succ: ${succ}")

      println(s"threeZeros: ${threeZeros}")
    }
  }
}
