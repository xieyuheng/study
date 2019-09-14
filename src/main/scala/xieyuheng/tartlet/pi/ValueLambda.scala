package xieyuheng.tartlet

case class ValueLambda(closure: Closure) extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    t match {
      case ValuePi(argType, retType) => {
        val freshName = Util.freshen(ctx.names, retType.name)
        val arg = TheNeutral(argType, NeutralVar(freshName))
        for {
          bodyValue <- Apply.exe(this, arg)
          realRetType <- retType.apply(arg)
          body <- bodyValue.readback(
            ctx.ext(freshName, Bind(argType)),
            realRetType)
        } yield Lambda(freshName, body)
      }
      case _ =>
        Left(Err(s"type of Lambda should be Pi: ${t}"))
    }
}
