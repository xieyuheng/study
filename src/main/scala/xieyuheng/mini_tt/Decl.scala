package xieyuheng.mini_tt

sealed trait Decl
final case class Let(pattern: Pattern, t: Exp, e: Exp) extends Decl
final case class LetRec(pattern: Pattern, t: Exp, e: Exp) extends Decl
