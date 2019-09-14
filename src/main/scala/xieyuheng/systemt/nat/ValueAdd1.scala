package xieyuheng.systemt

case class ValueAdd1(prev: Value) extends Value {
  def readback(usedNames: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Nat =>
        for {
          prevExp <- prev.readback(usedNames, t)
        } yield Add1(prevExp)
      case _ => Left(Err(
        s"type of ValueAdd1 should be Nat: ${t}"))
    }
  }
}
