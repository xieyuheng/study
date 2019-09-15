package xieyuheng.tartlet

case class TheVal (t: Val, value: Val) {
  def readback_the_val(ctx: Ctx): Either[Err, Exp] = {
    value.readback_val(ctx, t)
  }
}
