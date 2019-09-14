package xieyuheng.minitt

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Fn(pat: Pat, body: Exp) extends Exp
final case class Ap(fn: Exp, arg: Exp) extends Exp
final case class Pi(pat: Pat, arg_t: Exp, dep_t: Exp) extends Exp
final case class Sigma(pat: Pat, arg_t: Exp, dep_t: Exp) extends Exp
final case class Cons(car: Exp, cdr: Exp) extends Exp
final case class Car(pair: Exp) extends Exp
final case class Cdr(pair: Exp) extends Exp
final case class Data(tag: String, body: Exp) extends Exp
final case class Mat(mats: Map[String, Exp]) extends Exp
final case class Sum(mats: Map[String, Exp]) extends Exp
final case class Sole() extends Exp
final case class Trivial() extends Exp
final case class Univ() extends Exp
final case class Block(decl: Decl, body: Exp) extends Exp

sealed trait Pat {
  def maybe_name(): Option[String] = {
    this match {
      case PatVar(name) => Some(name)
      case _ => None
    }
  }
}
final case class PatVar(name: String) extends Pat
final case class PatCons(car: Pat, cdr: Pat) extends Pat
final case class PatSole() extends Pat
