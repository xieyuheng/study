import org.scalatest._
import xieyuheng.cicada._

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
    val env = Env()
      .defValue("x", TypeOfType("#x"))
      .defValue("y", TypeOfType("#y"))

    for {
      x <- eval(Var("x"), env)
      y <- eval(Var("y"), env)
    } {
      assert(x == TypeOfType("#x"))
      assert(y == TypeOfType("#y"))
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
    .defType("List", MultiMap("A" -> Type()), Map(
      "Null" -> MultiMap("A" -> Type()),
      "Cons" -> MultiMap(
        "A" -> Type(),
        "head" -> OfType(Var("A")),
        "tail" -> OfType(Ap(Var("List"), MultiMap("A" -> Var("A")))))))
    .defExp("cdr", Fn(
      args = MultiMap(
        "list" -> OfType(Var("List"))),
      ret = OfType(Var("List")),
      body = Field(Var("list"), "tail")))
    .defFn("append",
      args = MultiMap(
        "ante" -> OfType(Var("List")),
        "succ" -> OfType(Var("List"))),
      ret = OfType(Var("List")),
      body = Case(Var("ante"), MultiMap(
        "Null" -> Var("succ"),
        "Cons" -> Ap(Var("Cons"), MultiMap(
          "A" -> Field(Var("ante"), "A"),
          "head" -> Field(Var("ante"), "head"),
          "tail" -> Ap(Var("append"), MultiMap(
            "ante" -> Field(Var("ante"), "tail"),
            "succ" -> Var("succ"))))))))

  // .defFn("append",
  //   args = $(
  //     "ante" -> OfType("List"),
  //     "succ" -> OfType("List")),
  //   ret = OfType("List"),
  //   body = Case("ante", $(
  //     "Null" -> "succ",
  //     "Cons" -> Ap("Cons", $(
  //       "A" -> ("ante" dot "A"),
  //       "head" -> ("ante" dot "head"),
  //       "tail" -> Ap("append", $(
  //         "ante" -> ("ante" dot "tail"),
  //         "succ" -> "succ")))))))

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
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> zero,
        "tail" -> Ap(Var("Cons"), MultiMap(
          "A" -> Var("Nat"),
          "head" -> zero,
          "tail" -> Ap(Var("Cons"), MultiMap(
            "A" -> Var("Nat"),
            "head" -> zero,
            "tail" -> Var("Null")))))))

    pp(threeZeros, module)

    val one = Ap(Var("Succ"), MultiMap("prev" -> zero))

    val zeroAndOne =
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> zero,
        "tail" -> Ap(Var("Cons"), MultiMap(
          "A" -> Var("Nat"),
          "head" -> one,
          "tail" -> Var("Null")))))

    pp(zeroAndOne, module)

    pp(
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> Var("Zero"),
        "tail" -> Var("Null"))),
      module)

    pp(
      Ap(Var("Cons"), MultiMap(
        "A" -> Var("Nat"),
        "head" -> Var("Zero"),
        "tail" -> Ap(Var("Null"), MultiMap("A" -> Var("Nat"))))),
      module)

    val twoZeros = Ap(Var("cdr"), MultiMap(
      "list" -> threeZeros))

    val oneZero = Ap(Var("cdr"), MultiMap(
      "list" -> twoZeros))

    pp(twoZeros, module)
    pp(oneZero, module)

    pp(Ap(Var("append"), MultiMap(
      "ante" -> threeZeros,
      "succ" -> threeZeros)),
      module)
  }
}
