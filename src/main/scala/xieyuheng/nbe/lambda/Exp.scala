package xieyuheng.lambda

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(rator: Exp, arg: Exp) extends Exp
final case class Fn(name: String, body: Exp) extends Exp
final case class Block(decl: Decl, body: Exp) extends Exp
