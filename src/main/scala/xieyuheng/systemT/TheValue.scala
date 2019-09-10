package xieyuheng.systemT

case class TheValue(t: Type, value: Value) {
  def readbackTheValue(usedNames: Set [String]): Either[ErrorMsg, Exp] = {
    value.readback(usedNames, t)
  }
}
