package xieyuheng.tartlet

case object ValNat extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Nat)
}
