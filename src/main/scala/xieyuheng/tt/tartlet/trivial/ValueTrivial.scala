package xieyuheng.tt.tartlet

case object ValueTrivial extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Trivial)
}
