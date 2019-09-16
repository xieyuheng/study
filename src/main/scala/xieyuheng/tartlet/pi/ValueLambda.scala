package xieyuheng.tartlet

case class ValFn(clo: Clo) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(arg_t, ret_t) => {
        val fresh_name = util.freshen(ctx.names, ret_t.name)
        val arg = TheNeu(arg_t, NeuVar(fresh_name))
        for {
          bodyVal <- Ap.exe(this, arg)
          realRetType <- ret_t.apply(arg)
          body <- bodyVal.readback_val(
            ctx.ext(fresh_name, Bind(arg_t)),
            realRetType)
        } yield Fn(fresh_name, body)
      }
      case _ =>
        Left(Err(s"type of Fn should be Pi: ${t}"))
    }
}
