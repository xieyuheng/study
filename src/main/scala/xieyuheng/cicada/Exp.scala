package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class The(t: Exp) extends Exp
final case class Case(target: Exp, map: MultiMap[String, Exp]) extends Exp
final case class Dot(target: Exp, fieldName: String) extends Exp
final case class Pi(args: MultiMap[String, Exp], ret: Exp) extends Exp
final case class Fn(args: MultiMap[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, args: MultiMap[String, Exp]) extends Exp
