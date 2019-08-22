package xieyuheng.untyped

trait Value {
  def readBack(usedNames: Set[String]): Either[ErrorMsg, Exp]
}
