package xieyuheng.tartlet

case class ValAdd1 (prev: Val) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      prev_exp <- prev.readback_val(ctx, t)
    } yield Add1(prev_exp)
}
