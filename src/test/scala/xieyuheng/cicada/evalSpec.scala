import org.scalatest._
import xieyuheng.cicada._
import xieyuheng.cicada.dsl._

class evalSpec extends FlatSpec with Matchers {
  "eval" should "eval Type to TypeOfType" in {
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
    val xId = Id("x")
    val yId = Id("y")
    val env = Env()
      .defValue("x", TypeOfType(xId))
      .defValue("y", TypeOfType(yId))

    for {
      x <- eval(Var("x"), env)
      y <- eval(Var("y"), env)
    } {
      assert(x == TypeOfType(xId))
      assert(y == TypeOfType(yId))
    }
  }

  def pp(exp: Exp, env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${Pretty.fromValue(value, 0)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }

  val NatModule = Env()
    .defType("Nat", MultiMap(), Map(
      "Zero" -> MultiMap(),
      "Succ" -> MultiMap("prev" -> Var("Nat"))))

  it should "eval NatModule" in {
    val module = NatModule

    pp(Var("Nat"), module)
    pp(Var("Zero"), module)
    pp(Var("Succ"), module)
    pp(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), module)
    pp(Field(Ap(Var("Succ"), MultiMap("prev" -> Var("Zero"))), "prev"), module)
  }

  val ListModule = Env()
    .defType("List", $("A" -> Type()), Map(
      "Null" -> $("A" -> Type()),
      "Cons" -> $(
        "A" -> Type(),
        "head" -> The("A"),
        "tail" -> The(("List" ap $("A" -> "A"))))))
    .defExp("cdr", Fn(
      args = $(
        "list" -> The("List")),
      ret = The("List"),
      body = "list" dot "tail"))
    .defFn("append",
      args = $(
        "ante" -> The("List"),
        "succ" -> The("List")),
      ret = The("List"),
      body = Case("ante", $(
        "Null" -> "succ",
        "Cons" -> ("Cons" ap $(
          "A" -> ("ante" dot "A"),
          "head" -> ("ante" dot "head"),
          "tail" -> ("append" ap $(
            "ante" -> ("ante" dot "tail"),
            "succ" -> "succ")))))))

  it should "eval ListModule" in {
    val module = ListModule.importAll(NatModule)

    pp(Var("List"), module)
    pp(Var("Null"), module)
    pp(Var("Cons"), module)

    pp(Var("Nat"), module)
    pp(Var("Zero"), module)
    pp(Var("Succ"), module)

    val zero = Var("Zero")

    val threeZeros =
      "Cons" ap $(
        "A" -> "Nat",
        "head" -> zero,
        "tail" -> ("Cons" ap $(
          "A" -> "Nat",
          "head" -> zero,
          "tail" -> ("Cons" ap $(
            "A" -> "Nat",
            "head" -> zero,
            "tail" -> "Null")))))

    pp(threeZeros, module)

    val one = "Succ" ap $("prev" -> zero)

    val zeroAndOne =
      "Cons" ap $(
        "A" -> "Nat",
        "head" -> zero,
        "tail" -> ("Cons" ap $(
          "A" -> "Nat",
          "head" -> one,
          "tail" -> "Null")))

    pp(zeroAndOne, module)

    pp("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> "Null"),
      module)

    pp("Cons" ap $(
        "A" -> "Nat",
        "head" -> "Zero",
        "tail" -> ("Null" ap $("A" -> "Nat"))),
      module)

    val twoZeros = "cdr" ap $(
      "list" -> threeZeros)

    val oneZero = "cdr" ap $(
      "list" -> twoZeros)

    pp(twoZeros, module)
    pp(oneZero, module)

    pp("append" ap $(
      "ante" -> threeZeros,
      "succ" -> threeZeros),
      module)
  }
}
