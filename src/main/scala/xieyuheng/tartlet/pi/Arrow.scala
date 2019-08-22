package xieyuheng.tartlet

object Arrow {
  def apply(
    argType: Exp,
    retType: Exp,
  ): Type = Pi("_", argType, retType)
}
