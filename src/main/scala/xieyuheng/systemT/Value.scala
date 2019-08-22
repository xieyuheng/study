package xieyuheng.systemT

trait Value {
  def readBack (usedNames: Set[String], t: Type): Either[ErrorMsg, Exp]
}
