package xieyuheng.tartlet

trait Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp]
}
