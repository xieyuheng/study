package xieyuheng.hindley_milner

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(fn: Exp, arg: Exp) extends Exp
final case class Fn(argName: String, body: Exp) extends Exp
final case class Let(argName: String, arg: Exp, body: Exp) extends Exp

sealed trait Type
final case class TypeVar(name: String) extends Type
final case class Cons(name: String, args: List[Type]) extends Type
final case class Forall(argName: String, t: Type) extends Type

// TODO
// equality of types with alpha-conversion

case class Ctx(map: Map[String, Type])
