package xieyuheng.tartlet

case object ValueSame extends Value {
  def readback(ctx: Ctx, _t: Value): Either[ErrorMsg, Exp] = {
    Right(Same)
  }
}
