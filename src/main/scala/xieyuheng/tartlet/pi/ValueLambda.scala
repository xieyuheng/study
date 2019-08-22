package xieyuheng..tartlet

case class ValueLambda(closure: Closure) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    t match {
      case ValuePi(argType, retType) => {
        val freshName = Util.freshen(ctx.names, retType.name)
        val arg = TheNeutral(argType, NeutralVar(freshName))
        for {
          bodyValue <- Apply.exe(this, arg)
          realRetType <- retType.apply(arg)
          body <- bodyValue.readBack(
            ctx.ext(freshName, Bind(argType)),
            realRetType)
        } yield Lambda(freshName, body)
      }
      case _ =>
        Left(ErrorMsg(s"type of Lambda should be Pi: ${t}"))
    }
}
