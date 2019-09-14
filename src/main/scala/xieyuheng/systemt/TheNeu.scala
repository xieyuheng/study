package xieyuheng.systemt

case class TheNeu(t: Type, neutral: Neu) extends Val {
  def readback (used_names: Set [String], t: Type): Either[Err, Exp] = {
    neutral.readback_neu (used_names)
  }
}
