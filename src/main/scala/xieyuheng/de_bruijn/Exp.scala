package xieyuheng.de_bruijn

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Type, body: Exp) extends Exp
final case class Atom(name: String, str: String) extends Exp

sealed trait Type
final case class TypeAtom(name: String) extends Type
final case class TypeArrow(arg_t: Type, ret_t: Type) extends Type
