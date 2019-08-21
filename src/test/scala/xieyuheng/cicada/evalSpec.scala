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

  it should "eval prelude.nat" in {
    implicit val module = prelude.nat

    ep("Nat")
    ep("Zero")
    ep("Succ")
    ep("Succ" ap $("prev" -> "Zero"))
    ep("Succ" ap $("prev" -> "Zero") dot "prev")
  }

  it should "eval prelude.list" in {
    implicit val module = prelude.list.importAll(prelude.nat)

    ep("List")
    ep("Null")
    ep("Cons")

    ep("Nat")
    ep("Zero")
    ep("Succ")

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

    ep(threeZeros)

    val one = "Succ" ap $("prev" -> zero)

    val zeroAndOne =
      "Cons" ap $(
        "A" -> "Nat",
        "head" -> zero,
        "tail" -> ("Cons" ap $(
          "A" -> "Nat",
          "head" -> one,
          "tail" -> "Null")))

    ep(zeroAndOne)

    ep("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> "Null"))

    ep("Cons" ap $(
      "A" -> "Nat",
      "head" -> "Zero",
      "tail" -> ("Null" ap $("A" -> "Nat"))))

    val twoZeros = "cdr" ap $(
      "list" -> threeZeros)

    val oneZero = "cdr" ap $(
      "list" -> twoZeros)

    ep(twoZeros)
    ep(oneZero)

    ep("list_append" ap $(
      "ante" -> threeZeros,
      "succ" -> threeZeros))
  }

  it should "eval prelude.vec" in {
    implicit val module = prelude.vec

    ep("Vec")
    ep("NullVec")
    ep("ConsVec")
  }
}
