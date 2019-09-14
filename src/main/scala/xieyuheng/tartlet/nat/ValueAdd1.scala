package xieyuheng.tartlet

case class ValAdd1 (prev: Val) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      prevExp <- prev.readback(ctx, t)
    } yield Add1(prevExp)
}
