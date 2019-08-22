package xieyuheng.tartlet

case class TheValue (t: Value, value: Value) {
  def readBackTheValue(ctx: Ctx): Either[ErrorMsg, Exp] = {
    value.readBack(ctx, t)
  }
}
