package xieyuheng.tt.minitt

sealed trait Exp {
  def -:(ret: Exp) = Pi(U, ret, this)
  def *(cdr: Exp) = Sigma(U, this, cdr)
  def $(arg: Exp) = Apply(this, arg)
}
final case class Var(name: String) extends Exp

final case class Fn(pattern: Pattern, body: Exp) extends Exp
final case class Apply(fun: Exp, arg: Exp) extends Exp
final case class Pi(pattern: Pattern, arg: Exp, ret: Exp) extends Exp

final case class Cons(car: Exp, cdr: Exp) extends Exp
final case class Car(pair: Exp) extends Exp
final case class Cdr(pair: Exp) extends Exp
final case class Sigma(pattern: Pattern, car: Exp, cdr: Exp) extends Exp

final case class Data(tag: String, body: Exp) extends Exp
final case class Case(choices: Map[String, Exp]) extends Exp
object Case {
  def apply(pairs: (String, Exp)*) : Case = Case(Map(pairs: _*))
}
final case class Sum(choices: Map[String, Exp]) extends Exp
object Sum {
  def apply(pairs: (String, Exp)*) : Sum = Sum(Map(pairs: _*))
}

final case object Sole extends Exp
final case object Trivial extends Exp

final case object Universe extends Exp

final case class Seq(decl: Decl, next: Exp) extends Exp

sealed trait Pattern
final case class V(name: String) extends Pattern
final case class P(car: Pattern, cdr: Pattern) extends Pattern
final case object U extends Pattern

sealed trait Decl
final case class Let(name: String, t: Exp, body: Exp) extends Decl
final case class LetRec(name: String, t: Exp, body: Exp) extends Decl

object syntax {
  def Arrow(arg: Exp, ret: Exp) = Pi(U, arg, ret)
  def Pair(car: Exp, cdr: Exp) = Sigma(U, car, cdr)

  implicit def VarFromString(name: String) = Var(name)
  implicit def VFromString(name: String) = V(name)
}

object examples extends App {
  import syntax._

  val id = Let("id",
    Pi("A", Universe, Arrow("A", "A")),
    Fn("A", Fn("x", "x")))

  val Bool = Let("Bool", Universe, Sum(
    "true" -> Trivial,
    "false" -> Trivial))

  val elimBool = Let("elimBool",
    Pi("C", Arrow("Bool", Universe),
      Arrow(Apply("C", Data("true", Sole)),
        Arrow(Apply("C", Data("false", Sole)),
          Pi("b", "Bool", Apply("C", "b"))))),
    Fn("C", Fn("h0", Fn("h1", Case(
      "true" -> Fn("_", "h0"),
      "false" -> Fn("_", "h1"))))))

  val elimBool2 = Let("elimBool",
    Pi("C", "Bool" -: Universe,
      ("C" $ Data("true", Sole)) -:
        ("C" $ Data("false", Sole)) -:
        Pi("b", "Bool",  ("C" $ "b"))),
    Fn("C", Fn("h0", Fn("h1", Case(
      "true" -> Fn("_", "h0"),
      "false" -> Fn("_", "h1"))))))

  assert("a" -: "b" -: "c" == "a" -: ("b" -: "c"))
  assert(elimBool == elimBool2)

  val Nat = LetRec("Nat",
    Universe,
    Sum(
      "Zero" -> Trivial,
      "Succ" -> "Nat"))

  val List = LetRec("List",
    Universe -: Universe,
    Fn("A", Sum(
      "Nil" -> Trivial,
      "Cons" -> "A" * ("List" $ "A"))))

  val natrec = LetRec("natrec",
    Pi("C", "Nat" -: Universe,
      ("C" $ Data("Zero", Sole)) -:
        Pi("n", "Nat", ("C" $ "n") -: ("C" $ Data("Succ", "n"))) -:
        Pi("n", "Nat", ("C" $ "n"))),
    Fn("C", Fn("a", Fn("g", Case(
      "Zero" -> Fn("_", "a"),
      "Succ" -> Fn("prev", "g" $ "prev" $ ("natrec" $ "C" $ "a" $ "g" $ "prev")))))))

  val add = LetRec("add",
    "Nat" -: "Nat" -: "Nat",
    Fn("x", Case(
      "Zero" -> Fn("_", "x"),
      "Succ" -> Fn("prev", Data("Succ", "add" $ "x" $ "prev")))))

  val eqNat = LetRec("eqNat",
    "Nat" -: "Nat" -: "Bool",
    Case(
      "Zero" -> Fn("_", Case(
        "Zero" -> Fn("_", Data("true", Sole)),
        "Succ" -> Fn("_", Data("false", Sole)))),
      "Succ" -> Fn("x", Case(
        "Zero" -> Fn("_", Data("false", Sole)),
        "Succ" -> Fn("y", "eqNat" $ "x" $ "y")))))
}
