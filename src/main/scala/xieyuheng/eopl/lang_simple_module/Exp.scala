package xieyuheng.eopl.lang_simple_module

sealed trait Exp
final case class Var(name: String) extends Exp
