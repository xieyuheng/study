package xieyuheng.tartlet

case class ValueSigma (
  carType: Value,
  cdrType: Closure,
) extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    val freshName = Util.freshen(ctx.names, cdrType.name)
    for {
      carTypeExp <- carType.readback(ctx, ValueUniverse)
      cdrTypeExpValue <- cdrType.apply(
        TheNeutral(carType, NeutralVar(freshName)))
      cdrTypeExp <- cdrTypeExpValue.readback(
        ctx.ext(freshName, Bind(carType)), ValueUniverse)
    } yield Sigma(freshName, carTypeExp, cdrTypeExp)
  }
}
