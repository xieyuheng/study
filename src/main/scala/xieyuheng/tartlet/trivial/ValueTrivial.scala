package xieyuheng.tartlet

case object ValueTrivial extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    Right(Trivial)
}
