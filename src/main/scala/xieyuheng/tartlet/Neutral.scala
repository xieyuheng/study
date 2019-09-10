package xieyuheng.tartlet

trait Neutral {
  def readbackNeutral (ctx: Ctx): Either[ErrorMsg, Exp]
}
