package xieyuheng.minitt

sealed trait Exp {
  def ->:(arg: Exp) = Pi(EmptyPattern, arg, this)
  def *(cdr: Exp) = Sigma(EmptyPattern, this, cdr)
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
final case class Choice(choices: Map[String, Exp]) extends Exp
final case class Sum(choices: Map[String, Exp]) extends Exp
final case object Sole extends Exp
final case object Trivial extends Exp
final case object U extends Exp
final case class Seq(decl: Decl, next: Exp) extends Exp

sealed trait Pattern
final case class VarPattern(name: String) extends Pattern
final case class ConsPattern(car: Pattern, cdr: Pattern) extends Pattern
final case object EmptyPattern extends Pattern

sealed trait Decl
final case class Let(name: String, t: Exp, body: Exp) extends Decl
final case class LetRec(name: String, t: Exp, body: Exp) extends Decl

object expDSL {
  def choice(pairs: (String, Exp)*) : Choice = Choice(Map(pairs: _*))
  def sum(pairs: (String, Exp)*) : Sum = Sum(Map(pairs: _*))

  implicit def VarFromString(name: String) = Var(name)
  implicit def VarPatternFromString(name: String) = VarPattern(name)
}

object example extends App {
  import expDSL._

  // id : (A : U) -> A -> A
  // id = (A, x) => x

  Let("id",
    Pi("A", U, "A" ->: "A"),
    Fn("A", Fn("x", "x")))

  // Bool : U
  // Bool = sum {
  //   true
  //   false
  // }

  Let("Bool", U,
    sum(
      "true" -> Trivial,
      "false" -> Trivial))

  // elimBool : (C : Bool -> U) -> C true -> C false -> (b : Bool) -> C b
  // elimBool = (C, h0, h1) => choice {
  //   true => h0
  //   false => h1
  // }

  Let("elimBool",
    Pi("C", "Bool" ->: U,
      ("C" $ Data("true", Sole)) ->:
        ("C" $ Data("false", Sole)) ->:
        Pi("b", "Bool", "C" $ "b")),
    Fn("C", Fn("h0", Fn("h1", choice(
      "true" -> Fn("_", "h0"),
      "false" -> Fn("_", "h1"))))))

  // Nat : U
  // Nat = sum {
  //   zero
  //   succ Nat
  // }

  LetRec("Nat", U,
    sum(
      "zero" -> Trivial,
      "succ" -> "Nat"))

  // List : U -> U
  // List = A => sum {
  //   nil
  //   cons A List A
  // }

  LetRec("List", U ->: U,
    Fn("A", sum(
      "nil" -> Trivial,
      "cons" -> "A" * ("List" $ "A"))))

  // natrec : (C : Nat -> U) -> C zero -> ((n : Nat) -> C n -> C (succ n)) ->
  //          (n : Nat) -> C n
  // natrec = (C, a, g) => choice {
  //   zero => a
  //   succ prev => g prev (natrec C a g prev)
  // }

  LetRec("natrec",
    Pi("C", "Nat" ->: U,
      ("C" $ Data("zero", Sole)) ->:
        Pi("n", "Nat", ("C" $ "n") ->: ("C" $ Data("succ", "n"))) ->:
        Pi("n", "Nat", ("C" $ "n"))),
    Fn("C", Fn("a", Fn("g", choice(
      "zero" -> Fn("_", "a"),
      "succ" -> Fn("prev", "g" $ "prev" $ ("natrec" $ "C" $ "a" $ "g" $ "prev")))))))

  // add : Nat -> Nat -> Nat
  // add = x => choice {
  //   zero => x
  //   succ prev => succ (add x prev)
  // }

  LetRec("add",
    "Nat" ->: "Nat" ->: "Nat",
    Fn("x", choice(
      "zero" -> Fn("_", "x"),
      "succ" -> Fn("prev", Data("succ", "add" $ "x" $ "prev")))))

  // eqNat : Nat -> Nat -> Bool
  // eqNat = choice {
  //   zero => choice {
  //     zero => true
  //     succ _ => false
  //   }
  //   succ x => choice {
  //     zero => false
  //     succ y => eqNat x y
  //   }
  // }

  LetRec("eqNat",
    "Nat" ->: "Nat" ->: "Bool",
    choice(
      "zero" -> Fn("_", choice(
        "zero" -> Fn("_", Data("true", Sole)),
        "succ" -> Fn("_", Data("false", Sole)))),
      "succ" -> Fn("x", choice(
        "zero" -> Fn("_", Data("false", Sole)),
        "succ" -> Fn("y", "eqNat" $ "x" $ "y")))))

}

sealed trait Value
// TODO
