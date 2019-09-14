package xieyuheng.systemt

case object ValZero extends Val {
  def readback(usedNames: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Nat =>
        Right(Zero)
      case _ => Left(Err(
        s"type of ValZero should be Nat: ${t}"))
    }
  }
}
