package xieyuheng.tartlet

case object ValTrivial extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Trivial)
}
