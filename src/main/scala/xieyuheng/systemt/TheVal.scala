package xieyuheng.systemt

case class TheVal(t: Type, value: Val) {
  def readback_the_val(used_names: Set [String]): Either[Err, Exp] = {
    value.readback(used_names, t)
  }
}
