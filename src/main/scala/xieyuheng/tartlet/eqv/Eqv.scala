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

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Eqv(t2, from2, to2) => {
        t.alpha_eq(t2, this_map, that_map) &&
        from.alpha_eq(from2, this_map, that_map) &&
        to.alpha_eq(to2, this_map, that_map)
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
      typeVal <- t.eval(ctx.to_env)
      from <- from.check(ctx, typeVal)
      to <- to.check(ctx, typeVal)
    } yield The(Universe, Eqv(t, from, to))
  }
}
