package xieyuheng.systemt

trait Neutral {
  def readback_neu (usedNames: Set[String]): Either[Err, Exp]
}
