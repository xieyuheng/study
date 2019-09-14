package xieyuheng.systemt

trait Value {
  def readback (usedNames: Set[String], t: Type): Either[Err, Exp]
}
