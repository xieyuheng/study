package xieyuheng.systemT

trait Neutral {
  def readbackNeutral (usedNames: Set[String]): Either[ErrorMsg, Exp]
}
