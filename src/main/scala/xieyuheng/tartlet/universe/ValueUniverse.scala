package xieyuheng.tartlet

case object ValueUniverse extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Universe)
}
