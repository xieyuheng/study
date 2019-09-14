package xieyuheng.tartlet

case object ValUniverse extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Universe)
}
