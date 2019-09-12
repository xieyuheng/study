package xieyuheng.minitt

sealed trait Decl
final case class Let(pat: Pat, t: Exp, e: Exp) extends Decl
final case class Letrec(pat: Pat, t: Exp, e: Exp) extends Decl
