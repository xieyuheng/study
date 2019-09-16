package xieyuheng.tartlet

case class The (
  t: Exp,
  value: Exp,
) extends Exp {
  def eval(env: Env): Either[Err, Val] =
    value.eval(env)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case The(t2, value2) =>
        (t, t2) match {
          case (Absurd, Absurd) =>
            true
          case _ =>
            if (t.alpha_eq(t2, this_map, that_map)
              && value.alpha_eq(value2, this_map, that_map)) {
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
      tVal <- t.eval(ctx.to_env)
      value <- value.check(ctx, tVal)
    } yield The(t, value)

  def check(ctx: Ctx, t: Val): Either[Err, Exp] = {
    for {
      the <- infer(ctx)
      t2 <- the.t.eval(ctx.to_env)
      _ok <- util.conversion_check(ctx, ValUniverse, t, t2)
    } yield the.value
  }
}
