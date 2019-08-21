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

    ep("nat_t")
    ep("zero_t")
    ep("succ_t")
    ep("succ_t" ap $("prev" -> "zero_t"))
    ep("succ_t" ap $("prev" -> "zero_t") dot "prev")
  }

  it should "eval prelude.list" in {
    implicit val module = prelude.list.importAll(prelude.nat)

    ep("list_t")
    ep("null_t")
    ep("cons_t")

    ep("nat_t")
    ep("zero_t")
    ep("succ_t")

    val zero: Exp = "zero_t"

    val threeZeros =
      "cons_t" ap $(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> zero,
          "tail" -> ("cons_t" ap $(
            "A" -> "nat_t",
            "head" -> zero,
            "tail" -> "null_t")))))

    ep(threeZeros)

    val one = "succ_t" ap $("prev" -> zero)

    val zeroAndOne =
      "cons_t" ap $(
        "A" -> "nat_t",
        "head" -> zero,
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> one,
          "tail" -> "null_t")))

    ep(zeroAndOne)

    ep("cons_t" ap $(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> "null_t"))

    ep("cons_t" ap $(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> ("null_t" ap $("A" -> "nat_t"))))

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

    ep("vec_t")
    ep("null_vec_t")
    ep("cons_vec_t")
  }
}
