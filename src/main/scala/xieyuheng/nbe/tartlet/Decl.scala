package xieyuheng.tartlet

sealed trait Decl
final case class DeclLet(name: String, t: Exp, e: Exp) extends Decl
