package xieyuheng..tartlet

case object Sole extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] =
    Right(ValueSole)

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
  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    t match {
      case ValueTrivial =>
        Right(this)
      case _ =>
        Left(ErrorMsg(
          s"expected ValueTrivial, found: ${t}"))
    }
}
