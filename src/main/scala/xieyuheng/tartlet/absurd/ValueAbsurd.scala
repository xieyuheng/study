package xieyuheng.tartlet

case object ValAbsurd extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Absurd)
}
