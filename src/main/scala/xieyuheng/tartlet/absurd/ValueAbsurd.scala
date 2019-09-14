package xieyuheng.tartlet

case object ValAbsurd extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Absurd)
}
