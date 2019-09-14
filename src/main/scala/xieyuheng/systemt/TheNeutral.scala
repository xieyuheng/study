package xieyuheng.systemt

case class TheNeutral(t: Type, neutral: Neutral) extends Value {
  def readback (usedNames: Set [String], t: Type): Either[Err, Exp] = {
    neutral.readback_neu (usedNames)
  }
}
