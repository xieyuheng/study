package xieyuheng.tartlet

case class ValEqv (
  t: Val,
  from: Val,
  to: Val,
) extends Val {
  def readback(ctx: Ctx, _t: Val): Either[Err, Exp] = {
    for {
      t <- this.t.readback(ctx, ValUniverse)
      from <- from.readback(ctx, this.t)
      to <- to.readback(ctx, this.t)
    } yield Eqv(t, from, to)
  }
}
