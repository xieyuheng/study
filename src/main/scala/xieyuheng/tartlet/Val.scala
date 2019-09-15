package xieyuheng.tartlet

trait Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp]
}
