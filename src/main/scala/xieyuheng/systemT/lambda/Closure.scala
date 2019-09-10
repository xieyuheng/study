package xieyuheng.systemT

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Value {
  def readback(usedNames: Set[String], t: Type): Either[ErrorMsg, Exp] = {
    t match {
      case Arrow(argType, retType) =>
        val freshName = Util.freshen (usedNames, name)
        for {
          value <- Apply.exe(this, TheNeutral(argType, NeutralVar(freshName)))
          body2 <- value.readback(usedNames + freshName, retType)
        } yield Lambda(freshName, body2)
      case _ => Left(ErrorMsg(
        s"type of lambda should be arrow: ${t}"))
    }
  }
}
