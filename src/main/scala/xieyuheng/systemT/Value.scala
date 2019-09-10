package xieyuheng.systemT

trait Value {
  def readback (usedNames: Set[String], t: Type): Either[ErrorMsg, Exp]
}
