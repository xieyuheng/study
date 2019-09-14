package xieyuheng.tartlet

case object Zero extends Constructor {
  def eval(env: Env): Either[Err, Value] =
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
  def check(ctx: Ctx, t: Value): Either[Err, Exp] =
    t match {
      case ValueNat =>
        Right(this)
      case _ =>
        Left(Err(
          s"expected ValueNat, found: ${t}"))
    }
}
