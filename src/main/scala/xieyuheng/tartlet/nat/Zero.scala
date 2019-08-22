package xieyuheng.tartlet

case object Zero extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] =
    Right(ValueZero)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Zero => true
      case _ => false
    }
  }

  /*
   ---------------------
   ctx :- Zero <= Nat
   */
  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    t match {
      case ValueNat =>
        Right(this)
      case _ =>
        Left(ErrorMsg(
          s"expected ValueNat, found: ${t}"))
    }
}
