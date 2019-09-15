package xieyuheng.tartlet

case object ValSole extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Sole)
}
