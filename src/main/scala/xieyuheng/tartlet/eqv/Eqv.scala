package xieyuheng.tartlet

case class Eqv(
  t: Exp,
  from: Exp,
  to: Exp,
) extends Type {
  def eval(env: Env): Either[Err, Val] = {
    for {
      t <- t.eval(env)
      from <- from.eval(env)
      to <- to.eval(env)
    } yield ValEqv(t, from, to)
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Eqv(t2, from2, to2) => {
        t.alphaEq(t2, thisMap, thatMap) &&
        from.alphaEq(from2, thisMap, thatMap) &&
        to.alphaEq(to2, thisMap, thatMap)
      }
      case _ => false
    }
  }

  /*
    ctx :- T <= Universe
    ctx :- from <= T
    ctx :- to <= T
    --------------------
    ctx :- Eqv(T, from, to) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      t <- t.check(ctx, ValUniverse)
      typeVal <- t.eval(ctx.toEnv)
      from <- from.check(ctx, typeVal)
      to <- to.check(ctx, typeVal)
    } yield The(Universe, Eqv(t, from, to))
  }
}
