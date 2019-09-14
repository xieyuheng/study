package xieyuheng.tartlet

case class ValPi (
  argType: Val,
  retType: Closure,
) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] = {
    val freshName = util.freshen(ctx.names, retType.name)
    for {
      argTypeExp <- argType.readback(ctx, ValUniverse)
      retTypeExpVal <- retType.apply(
        TheNeu(argType, NeuVar(freshName)))
      retTypeExp <- retTypeExpVal.readback(
        ctx.ext(freshName, Bind(argType)), ValUniverse)
    } yield Pi(freshName, argTypeExp, retTypeExp)
  }
}
