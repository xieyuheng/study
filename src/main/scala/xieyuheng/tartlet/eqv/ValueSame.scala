package xieyuheng.tartlet

case object ValueSame extends Value {
  def readBack(ctx: Ctx, _t: Value): Either[ErrorMsg, Exp] = {
    Right(Same)
  }
}
