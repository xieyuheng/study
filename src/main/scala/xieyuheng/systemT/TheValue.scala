package xieyuheng..systemT

case class TheValue(t: Type, value: Value) {
  def readBackTheValue(usedNames: Set [String]): Either[ErrorMsg, Exp] = {
    value.readBack(usedNames, t)
  }
}
