package xieyuheng.tartlet

case object ValueNat extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Nat)
}
