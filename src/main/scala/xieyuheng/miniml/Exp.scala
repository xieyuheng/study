package xieyuheng.miniml

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(fn: Exp, arg: Exp) extends Exp
final case class Fn(arg_name: String, body: Exp) extends Exp
final case class Let(arg_name: String, arg: Exp, body: Exp) extends Exp

sealed trait Type
final case class TypeVar(name: String) extends Type
final case class Cons(name: String, args: List[Type]) extends Type
final case class Forall(arg_name: String, t: Type) extends Type

// TODO
// equality of types with alpha-conversion

case class Ctx(map: Map[String, Type])
