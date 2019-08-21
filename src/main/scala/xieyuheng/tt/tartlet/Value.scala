package xieyuheng.tt.tartlet

trait Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp]
}
