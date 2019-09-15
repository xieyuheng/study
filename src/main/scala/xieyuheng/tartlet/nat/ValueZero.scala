package xieyuheng.tartlet

case object ValZero extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Zero)
}
