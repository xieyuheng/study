package xieyuheng.tartlet

case object ValueUniverse extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Universe)
}
