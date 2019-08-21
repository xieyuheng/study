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

  def pp(exp: Exp)(implicit env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${Pretty.fromValue(value, 0)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }

  val NatModule = Env()
    .defType("Nat", $(),
      members = $(
        "Zero" -> $(),
        "Succ" -> $("prev" -> "Nat")))

  it should "eval NatModule" in {
    implicit val module = NatModule

    pp("Nat")
    pp("Zero")
    pp("Succ")
    pp("Succ" ap $("prev" -> "Zero"))
    pp("Succ" ap $("prev" -> "Zero") dot "prev")
  }

  val ListModule = Env()
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
    implicit val module = ListModule.importAll(NatModule)

    pp(Var("List"))
    pp(Var("Null"))
    pp(Var("Cons"))

    pp(Var("Nat"))
    pp(Var("Zero"))
    pp(Var("Succ"))

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

    pp(threeZeros)

    val one = "Succ" ap $("prev" -> zero)

    val zeroAndOne =
      "Cons" ap $(
        "A" -> "Nat",
        "head" -> zero,
        "tail" -> ("Cons" ap $(
          "A" -> "Nat",
          "head" -> one,
          "tail" -> "Null")))

    pp(zeroAndOne)

    pp("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> "Null"))

    pp("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> ("Null" ap $("A" -> "Nat"))))

    val twoZeros = "cdr" ap $(
      "list" -> threeZeros)

    val oneZero = "cdr" ap $(
      "list" -> twoZeros)

    pp(twoZeros)
    pp(oneZero)

    pp("append" ap $(
      "ante" -> threeZeros,
      "succ" -> threeZeros))
  }

  val VecModule = Env().importAll(NatModule)
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

  it should "eval VecModule" in {
    implicit val module = VecModule

    pp("Vec")
    pp("NullVec")
    pp("ConsVec")
  }
}
