package xieyuheng.tartlet

case object Same extends Constructor {
  def eval(env: Env): Either[Err, Val] = {
    Right(ValSame)
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Same => true
      case _ => false
    }
  }

  /*
    ctx :- conversionCheck(T, from, to)
    ---------------------
    ctx :- Same <= Eqv(T, from, to)
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] = {
    t match {
      case ValEqv(typeVal, from, to) =>
        for {
          _ok <- util.conversionCheck(ctx, typeVal, from, to)
        } yield this
      case _ =>
        Left(Err(
          s"expected ValEqv(typeVal, from, to), found: ${t}"))
    }
  }
}
