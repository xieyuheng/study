package xieyuheng.tartlet

case object Zero extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValZero)

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
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValNat =>
        Right(this)
      case _ =>
        Left(Err(
          s"expected ValNat, found: ${t}"))
    }
}
