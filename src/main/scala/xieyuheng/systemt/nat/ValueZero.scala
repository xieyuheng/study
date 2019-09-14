package xieyuheng.systemt

case object ValueZero extends Value {
  def readback(usedNames: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Nat =>
        Right(Zero)
      case _ => Left(Err(
        s"type of ValueZero should be Nat: ${t}"))
    }
  }
}
