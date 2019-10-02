package xieyuheng.simple.curry

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Fn(arg_name: String, body: Exp) extends Exp

object Exp {
  def free_variables(exp: Exp): Set[String] = {
    exp match {
      case Var(name: String) =>
        Set(name)
      case Ap(target: Exp, arg: Exp) =>
        free_variables(target) ++ free_variables(arg)
      case Fn(arg_name: String, body: Exp) =>
        free_variables(body) - arg_name
    }
  }
}

sealed trait Type
final case class Atom(name: String) extends Type
final case class Arrow(ante: Type, succ: Type) extends Type
