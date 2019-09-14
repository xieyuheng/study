package xieyuheng.tartlet

case class ValSigma (
  carType: Val,
  cdrType: Closure,
) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] = {
    val freshName = util.freshen(ctx.names, cdrType.name)
    for {
      carTypeExp <- carType.readback(ctx, ValUniverse)
      cdrTypeExpVal <- cdrType.apply(
        TheNeu(carType, NeuVar(freshName)))
      cdrTypeExp <- cdrTypeExpVal.readback(
        ctx.ext(freshName, Bind(carType)), ValUniverse)
    } yield Sigma(freshName, carTypeExp, cdrTypeExp)
  }
}
