package xieyuheng.tartlet

case object ValueZero extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Zero)
}
