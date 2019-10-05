package xieyuheng.de_bruijn

sealed trait Decl
final case class DeclLet(name: String, e: Exp) extends Decl
