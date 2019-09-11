package xieyuheng.minitt

sealed trait Decl
final case class Let(pattern: Pattern, t: Exp, e: Exp) extends Decl
final case class Letrec(pattern: Pattern, t: Exp, e: Exp) extends Decl
