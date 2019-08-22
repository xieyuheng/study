package xieyuheng..tartlet

case class ValuePi (
  argType: Value,
  retType: Closure,
) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    val freshName = Util.freshen(ctx.names, retType.name)
    for {
      argTypeExp <- argType.readBack(ctx, ValueUniverse)
      retTypeExpValue <- retType.apply(
        TheNeutral(argType, NeutralVar(freshName)))
      retTypeExp <- retTypeExpValue.readBack(
        ctx.ext(freshName, Bind(argType)), ValueUniverse)
    } yield Pi(freshName, argTypeExp, retTypeExp)
  }
}
