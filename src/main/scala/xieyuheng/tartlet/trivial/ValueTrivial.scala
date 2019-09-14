package xieyuheng.tartlet

case object ValTrivial extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Trivial)
}
