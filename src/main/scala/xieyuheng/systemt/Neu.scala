package xieyuheng.systemt

trait Neu {
  def readback_neu (usedNames: Set[String]): Either[Err, Exp]
}
