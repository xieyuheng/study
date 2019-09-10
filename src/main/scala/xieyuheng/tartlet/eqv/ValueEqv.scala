package xieyuheng.tartlet

case class ValueEqv (
  t: Value,
  from: Value,
  to: Value,
) extends Value {
  def readback(ctx: Ctx, _t: Value): Either[ErrorMsg, Exp] = {
    for {
      t <- this.t.readback(ctx, ValueUniverse)
      from <- from.readback(ctx, this.t)
      to <- to.readback(ctx, this.t)
    } yield Eqv(t, from, to)
  }
}
