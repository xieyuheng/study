package xieyuheng.tartlet

case class ValSigma (
  carType: Val,
  cdrType: Clo,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, cdrType.name)
    for {
      carTypeExp <- carType.readback_val(ctx, ValUniverse)
      cdrTypeExpVal <- cdrType.apply(
        TheNeu(carType, NeuVar(fresh_name)))
      cdrTypeExp <- cdrTypeExpVal.readback_val(
        ctx.ext(fresh_name, Bind(carType)), ValUniverse)
    } yield Sigma(fresh_name, carTypeExp, cdrTypeExp)
  }
}
