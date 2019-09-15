package xieyuheng.tartlet

case class ValPi (
  argType: Val,
  retType: Clo,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, retType.name)
    for {
      argTypeExp <- argType.readback_val(ctx, ValUniverse)
      retTypeExpVal <- retType.apply(
        TheNeu(argType, NeuVar(fresh_name)))
      retTypeExp <- retTypeExpVal.readback_val(
        ctx.ext(fresh_name, Bind(argType)), ValUniverse)
    } yield Pi(fresh_name, argTypeExp, retTypeExp)
  }
}
