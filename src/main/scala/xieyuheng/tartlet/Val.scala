package xieyuheng.tartlet

trait Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp]
}
