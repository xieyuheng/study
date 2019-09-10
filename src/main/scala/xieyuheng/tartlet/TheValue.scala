package xieyuheng.tartlet

case class TheValue (t: Value, value: Value) {
  def readbackTheValue(ctx: Ctx): Either[ErrorMsg, Exp] = {
    value.readback(ctx, t)
  }
}
