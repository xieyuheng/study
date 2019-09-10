package xieyuheng.tartlet

case class ValuePi (
  argType: Value,
  retType: Closure,
) extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    val freshName = Util.freshen(ctx.names, retType.name)
    for {
      argTypeExp <- argType.readback(ctx, ValueUniverse)
      retTypeExpValue <- retType.apply(
        TheNeutral(argType, NeutralVar(freshName)))
      retTypeExp <- retTypeExpValue.readback(
        ctx.ext(freshName, Bind(argType)), ValueUniverse)
    } yield Pi(freshName, argTypeExp, retTypeExp)
  }
}
