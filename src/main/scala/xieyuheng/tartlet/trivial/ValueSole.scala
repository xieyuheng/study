package xieyuheng.tartlet

case object ValueSole extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Sole)
}
