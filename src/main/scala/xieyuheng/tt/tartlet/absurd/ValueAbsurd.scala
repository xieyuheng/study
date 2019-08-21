package xieyuheng.tt.tartlet

case object ValueAbsurd extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Absurd)
}
