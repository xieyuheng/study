package xieyuheng.simple.church

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Type, body: Exp) extends Exp

sealed trait Type
final case class Atom(name: String) extends Type
final case class Arrow(ante: Type, succ: Type) extends Type