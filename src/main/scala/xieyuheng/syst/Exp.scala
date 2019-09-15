package xieyuheng.syst

sealed trait Exp
final case class Var(name: String) extends Exp
final case class The(t: Type, exp: Exp) extends Exp
final case class Zero() extends Exp
final case class Succ(prev: Exp) extends Exp
final case class NatRec(t: Type, target: Exp, base: Exp, step: Exp) extends Exp
final case class Fn(name: String, body: Exp) extends Exp
final case class Ap(rator: Exp, rand: Exp) extends Exp
final case class Block(decl: Decl, body: Exp) extends Exp
