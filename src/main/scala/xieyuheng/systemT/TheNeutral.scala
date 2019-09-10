package xieyuheng.systemT

case class TheNeutral(t: Type, neutral: Neutral) extends Value {
  def readback (usedNames: Set [String], t: Type): Either[ErrorMsg, Exp] = {
    neutral.readbackNeutral (usedNames)
  }
}
