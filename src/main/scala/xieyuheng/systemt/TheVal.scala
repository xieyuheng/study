package xieyuheng.systemt

case class TheVal(t: Type, value: Val) {
  def readback_the_val(usedNames: Set [String]): Either[Err, Exp] = {
    value.readback(usedNames, t)
  }
}
