package xieyuheng.tartlet

case class ValueAdd1 (prev: Value) extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    for {
      prevExp <- prev.readback(ctx, t)
    } yield Add1(prevExp)
}
