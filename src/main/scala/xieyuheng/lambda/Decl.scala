package xieyuheng.lambda

sealed trait Decl
final case class DeclLet(name: String, e: Exp) extends Decl
