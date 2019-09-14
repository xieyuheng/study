package xieyuheng.systemt

trait Val {
  def readback (usedNames: Set[String], t: Type): Either[Err, Exp]
}
