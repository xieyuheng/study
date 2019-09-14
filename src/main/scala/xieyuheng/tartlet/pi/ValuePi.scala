package xieyuheng.tartlet

case class ValPi (
  argType: Val,
  retType: Closure,
) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, retType.name)
    for {
      argTypeExp <- argType.readback(ctx, ValUniverse)
      retTypeExpVal <- retType.apply(
        TheNeu(argType, NeuVar(fresh_name)))
      retTypeExp <- retTypeExpVal.readback(
        ctx.ext(fresh_name, Bind(argType)), ValUniverse)
    } yield Pi(fresh_name, argTypeExp, retTypeExp)
  }
}
