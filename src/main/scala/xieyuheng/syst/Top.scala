package xieyuheng.syst

sealed trait Top
final case class TopDecl(decl: Decl) extends Top
final case class TopEval(exp: Exp) extends Top
final case class TopEq(x: Exp, y: Exp) extends Top
final case class TopNotEq(x: Exp, y: Exp) extends Top
