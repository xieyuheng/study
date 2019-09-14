package xieyuheng.tartlet

case class ValSigma (
  carType: Val,
  cdrType: Closure,
) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, cdrType.name)
    for {
      carTypeExp <- carType.readback(ctx, ValUniverse)
      cdrTypeExpVal <- cdrType.apply(
        TheNeu(carType, NeuVar(fresh_name)))
      cdrTypeExp <- cdrTypeExpVal.readback(
        ctx.ext(fresh_name, Bind(carType)), ValUniverse)
    } yield Sigma(fresh_name, carTypeExp, cdrTypeExp)
  }
}
