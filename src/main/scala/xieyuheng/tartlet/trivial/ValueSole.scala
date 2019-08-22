package xieyuheng..tartlet

case object ValueSole extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Sole)
}
