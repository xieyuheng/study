package xieyuheng.cicada

sealed trait Top
final case class TopDecl(decl: Decl) extends Top
final case class TopImportAll(path: String) extends Top
final case class TopShow(exp: Exp) extends Top
final case class TopEq(x: Exp, y: Exp) extends Top
final case class TopNotEq(x: Exp, y: Exp) extends Top
