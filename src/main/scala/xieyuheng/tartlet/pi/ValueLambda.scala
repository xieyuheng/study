package xieyuheng.tartlet

case class ValLambda(closure: Closure) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(argType, retType) => {
        val freshName = util.freshen(ctx.names, retType.name)
        val arg = TheNeu(argType, NeuVar(freshName))
        for {
          bodyVal <- Apply.exe(this, arg)
          realRetType <- retType.apply(arg)
          body <- bodyVal.readback(
            ctx.ext(freshName, Bind(argType)),
            realRetType)
        } yield Lambda(freshName, body)
      }
      case _ =>
        Left(Err(s"type of Lambda should be Pi: ${t}"))
    }
}
