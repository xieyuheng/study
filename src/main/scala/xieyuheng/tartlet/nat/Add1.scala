package xieyuheng.tartlet

case class Add1 (prev: Exp) extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] =
    for {
      prevValue <- prev.eval(env)
    } yield ValueAdd1(prevValue)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Add1(prev2) =>
        prev.alphaEq(prev2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- prev <= Nat
   ---------------------
   ctx :- Add1(prev) <= Nat
   */
  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    t match {
      case ValueNat =>
        for {
          prev <- prev.check(ctx, t)
        } yield Add1(prev)
      case _ =>
        Left(ErrorMsg(
          s"expected ValueAdd1, found: ${t}"))
    }
}
