package xieyuheng.simple

object curry {

  sealed trait Exp
  final case class Var(name: String) extends Exp
  final case class Ap(target: Exp, arg: Exp) extends Exp
  final case class Fn(arg_name: String, body: Exp) extends Exp

  sealed trait Type
  final case class TypeAtom(name: String) extends Type
  final case class TypeArrow(ante: Type, succ: Type) extends Type

}
