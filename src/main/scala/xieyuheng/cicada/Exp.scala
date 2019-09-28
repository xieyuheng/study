package xieyuheng.cicada

import pretty._

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type(level: Int) extends Exp
final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Choice(path: List[String], map: Map[String, Exp]) extends Exp
final case class Dot(target: Exp, field_name: String) extends Exp
final case class DotType(target: Exp, field_name: String) extends Exp
final case class Let(decl: Decl, body: Exp) extends Exp

object Exp {
  def from_path(path: List[String]): Exp = {
    assert(path.length > 0)
    val init: Exp = Var(path.head)
    path.tail.foldLeft(init) { case (exp, field_name) => Dot(exp, field_name) }
  }

}
