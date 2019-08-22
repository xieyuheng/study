package xieyuheng..tartlet

case class ValueEqv (
  t: Value,
  from: Value,
  to: Value,
) extends Value {
  def readBack(ctx: Ctx, _t: Value): Either[ErrorMsg, Exp] = {
    for {
      t <- this.t.readBack(ctx, ValueUniverse)
      from <- from.readBack(ctx, this.t)
      to <- to.readBack(ctx, this.t)
    } yield Eqv(t, from, to)
  }
}
