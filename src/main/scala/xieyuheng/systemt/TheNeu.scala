package xieyuheng.systemt

case class TheNeu(t: Type, neutral: Neu) extends Val {
  def readback (usedNames: Set [String], t: Type): Either[Err, Exp] = {
    neutral.readback_neu (usedNames)
  }
}
