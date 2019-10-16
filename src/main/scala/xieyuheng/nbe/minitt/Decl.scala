package xieyuheng.minitt

sealed trait Decl
final case class DeclLet(pat: Pat, t: Exp, e: Exp) extends Decl
final case class DeclLetrec(pat: Pat, t: Exp, e: Exp) extends Decl
