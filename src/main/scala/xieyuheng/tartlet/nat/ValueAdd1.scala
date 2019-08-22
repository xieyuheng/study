package xieyuheng.tartlet

case class ValueAdd1 (prev: Value) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    for {
      prevExp <- prev.readBack(ctx, t)
    } yield Add1(prevExp)
}
