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
      x <- eval("x", env)
      y <- eval("y", env)
    } {
      assert(x == TypeOfType(xId))
      assert(y == TypeOfType(yId))
    }
  }

  def repl(exp: Exp)(implicit env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${Pretty.fromValue(value, 0)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }

  it should "eval prelude.nat" in {
    implicit val module = prelude.nat

    repl("Nat")
    repl("Zero")
    repl("Succ")
    repl("Succ" ap $("prev" -> "Zero"))
    repl("Succ" ap $("prev" -> "Zero") dot "prev")
  }

  it should "eval prelude.list" in {
    implicit val module = prelude.list.importAll(prelude.nat)

    repl("List")
    repl("Null")
    repl("Cons")

    repl("Nat")
    repl("Zero")
    repl("Succ")

    val zero: Exp = "Zero"

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

    repl(threeZeros)

    val one = "Succ" ap $("prev" -> zero)

    val zeroAndOne =
      "Cons" ap $(
        "A" -> "Nat",
        "head" -> zero,
        "tail" -> ("Cons" ap $(
          "A" -> "Nat",
          "head" -> one,
          "tail" -> "Null")))

    repl(zeroAndOne)

    repl("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> "Null"))

    repl("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> ("Null" ap $("A" -> "Nat"))))

    val twoZeros = "cdr" ap $(
      "list" -> threeZeros)

    val oneZero = "cdr" ap $(
      "list" -> twoZeros)

    repl(twoZeros)
    repl(oneZero)

    repl("areplend" ap $(
      "ante" -> threeZeros,
      "succ" -> threeZeros))
  }

  it should "eval prelude.vec" in {
    implicit val module = prelude.vec

    repl("Vec")
    repl("NullVec")
    repl("ConsVec")
  }
}
