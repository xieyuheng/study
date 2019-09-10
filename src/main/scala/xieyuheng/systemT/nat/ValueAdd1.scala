package xieyuheng.systemT

case class ValueAdd1(prev: Value) extends Value {
  def readback(usedNames: Set[String], t: Type): Either[ErrorMsg, Exp] = {
    t match {
      case Nat =>
        for {
          prevExp <- prev.readback(usedNames, t)
        } yield Add1(prevExp)
      case _ => Left(ErrorMsg(
        s"type of ValueAdd1 should be Nat: ${t}"))
    }
  }
}
