package xieyuheng.tartlet

trait Neutral {
  def readback_neu (ctx: Ctx): Either[Err, Exp]
}
