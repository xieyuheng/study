package xieyuheng.tartlet

case class ValLambda(clo: Clo) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(argType, retType) => {
        val fresh_name = util.freshen(ctx.names, retType.name)
        val arg = TheNeu(argType, NeuVar(fresh_name))
        for {
          bodyVal <- Ap.exe(this, arg)
          realRetType <- retType.apply(arg)
          body <- bodyVal.readback_val(
            ctx.ext(fresh_name, Bind(argType)),
            realRetType)
        } yield Lambda(fresh_name, body)
      }
      case _ =>
        Left(Err(s"type of Lambda should be Pi: ${t}"))
    }
}
