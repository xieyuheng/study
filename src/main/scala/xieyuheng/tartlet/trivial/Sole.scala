package xieyuheng.tartlet

case object Sole extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValSole)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Sole => true
      case _ => false
    }
  }

  /*
   ---------------------
   ctx :- Sole <= Trivial
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValTrivial =>
        Right(this)
      case _ =>
        Left(Err(
          s"expected ValTrivial, found: ${t}"))
    }
}
