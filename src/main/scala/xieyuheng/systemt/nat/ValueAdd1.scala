package xieyuheng.systemt

case class ValAdd1(prev: Val) extends Val {
  def readback(usedNames: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Nat =>
        for {
          prevExp <- prev.readback(usedNames, t)
        } yield Add1(prevExp)
      case _ => Left(Err(
        s"type of ValAdd1 should be Nat: ${t}"))
    }
  }
}
