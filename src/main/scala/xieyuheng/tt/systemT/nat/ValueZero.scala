package xieyuheng.tt.systemT

case object ValueZero extends Value {
  def readBack(usedNames: Set[String], t: Type): Either[ErrorMsg, Exp] = {
    t match {
      case Nat =>
        Right(Zero)
      case _ => Left(ErrorMsg(
        s"type of ValueZero should be Nat: ${t}"))
    }
  }
}
