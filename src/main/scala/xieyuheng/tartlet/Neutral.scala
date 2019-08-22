package xieyuheng..tartlet

trait Neutral {
  def readBackNeutral (ctx: Ctx): Either[ErrorMsg, Exp]
}
