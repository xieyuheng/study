package xieyuheng.tartlet

trait Neu {
  def readback_neu (ctx: Ctx): Either[Err, Exp]
}
