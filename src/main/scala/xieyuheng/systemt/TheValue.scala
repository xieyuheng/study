package xieyuheng.systemt

case class TheValue(t: Type, value: Value) {
  def readback_the_val(usedNames: Set [String]): Either[Err, Exp] = {
    value.readback(usedNames, t)
  }
}
