package xieyuheng..systemT

trait Neutral {
  def readBackNeutral (usedNames: Set[String]): Either[ErrorMsg, Exp]
}
