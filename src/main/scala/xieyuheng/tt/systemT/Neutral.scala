package xieyuheng.tt.systemT

trait Neutral {
  def readBackNeutral (usedNames: Set[String]): Either[ErrorMsg, Exp]
}
