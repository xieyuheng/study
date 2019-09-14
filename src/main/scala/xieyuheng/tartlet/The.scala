package xieyuheng.tartlet

case class The (
  t: Exp,
  value: Exp,
) extends Exp {
  def eval(env: Env): Either[Err, Val] =
    value.eval(env)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case The(t2, value2) =>
        (t, t2) match {
          case (Absurd, Absurd) =>
            true
          case _ =>
            if (t.alphaEq(t2, thisMap, thatMap)
              && value.alphaEq(value2, thisMap, thatMap)) {
              true
            } else {
              false
            }
        }
      case _ =>
        false
    }
  }

  /*
   ctx :- T <= UNIVERSE
   ctx :- e <= T
   -----------------
   ctx :- e: T => T
   */
  def infer(ctx: Ctx): Either[Err, The] =
    for {
      t <- t.check(ctx, ValUniverse)
      tVal <- t.eval(ctx.toEnv)
      value <- value.check(ctx, tVal)
    } yield The(t, value)

  def check(ctx: Ctx, t: Val): Either[Err, Exp] = {
    for {
      the <- infer(ctx)
      t2 <- the.t.eval(ctx.toEnv)
      _ok <- util.conversionCheck(ctx, ValUniverse, t, t2)
    } yield the.value
  }
}
