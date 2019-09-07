package xieyuheng.minitt

sealed trait Decl
final case class Let(name: String, t: Exp, e: Exp) extends Decl
final case class LetRec(name: String, t: Exp, e: Exp) extends Decl
