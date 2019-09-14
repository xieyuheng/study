package xieyuheng.tartlet

case class Add1 (prev: Exp) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    for {
      prevVal <- prev.eval(env)
    } yield ValAdd1(prevVal)

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
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValNat =>
        for {
          prev <- prev.check(ctx, t)
        } yield Add1(prev)
      case _ =>
        Left(Err(
          s"expected ValAdd1, found: ${t}"))
    }
}
