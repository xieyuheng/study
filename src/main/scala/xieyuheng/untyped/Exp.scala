package xieyuheng.untyped

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(rator: Exp, rand: Exp) extends Exp
final case class Lambda(name: String, body: Exp) extends Exp
