package xieyuheng.tartlet

case class ValEqv (
  t: Val,
  from: Val,
  to: Val,
) extends Val {
  def readback_val(ctx: Ctx, _t: Val): Either[Err, Exp] = {
    for {
      t <- this.t.readback_val(ctx, ValUniverse)
      from <- from.readback_val(ctx, this.t)
      to <- to.readback_val(ctx, this.t)
    } yield Eqv(t, from, to)
  }
}
