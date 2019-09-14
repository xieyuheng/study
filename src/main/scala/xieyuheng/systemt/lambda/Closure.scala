package xieyuheng.systemt

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Val {
  def readback(usedNames: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Arrow(argType, retType) =>
        val freshName = util.freshen (usedNames, name)
        for {
          value <- Apply.exe(this, TheNeu(argType, NeuVar(freshName)))
          body2 <- value.readback(usedNames + freshName, retType)
        } yield Lambda(freshName, body2)
      case _ => Left(Err(
        s"type of lambda should be arrow: ${t}"))
    }
  }
}
