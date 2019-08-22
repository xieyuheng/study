package xieyuheng.systemT

case class TheNeutral(t: Type, neutral: Neutral) extends Value {
  def readBack (usedNames: Set [String], t: Type): Either[ErrorMsg, Exp] = {
    neutral.readBackNeutral (usedNames)
  }
}
