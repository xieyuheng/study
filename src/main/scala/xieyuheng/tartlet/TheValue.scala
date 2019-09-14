package xieyuheng.tartlet

case class TheValue (t: Value, value: Value) {
  def readback_the_val(ctx: Ctx): Either[Err, Exp] = {
    value.readback(ctx, t)
  }
}
