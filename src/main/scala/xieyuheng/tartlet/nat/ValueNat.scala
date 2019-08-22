package xieyuheng..tartlet

case object ValueNat extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Nat)
}
