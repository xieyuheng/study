package xieyuheng.tartlet

case object Same extends Constructor {
  def eval(env: Env): Either[Err, Value] = {
    Right(ValueSame)
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
  def check(ctx: Ctx, t: Value): Either[Err, Exp] = {
    t match {
      case ValueEqv(typeValue, from, to) =>
        for {
          _ok <- Util.conversionCheck(ctx, typeValue, from, to)
        } yield this
      case _ =>
        Left(Err(
          s"expected ValueEqv(typeValue, from, to), found: ${t}"))
    }
  }
}
