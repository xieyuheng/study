package xieyuheng.tartlet

case object ValueAbsurd extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    Right(Absurd)
}
