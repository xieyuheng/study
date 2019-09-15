package xieyuheng.tartlet

case object ValSame extends Val {
  def readback_val(ctx: Ctx, _t: Val): Either[Err, Exp] = {
    Right(Same)
  }
}
