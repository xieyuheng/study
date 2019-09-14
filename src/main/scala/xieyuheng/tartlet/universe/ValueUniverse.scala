package xieyuheng.tartlet

case object ValueUniverse extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    Right(Universe)
}
