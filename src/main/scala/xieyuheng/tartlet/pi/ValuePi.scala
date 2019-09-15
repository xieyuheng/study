package xieyuheng.tartlet

case class ValPi (
  arg_t: Val,
  ret_t: Clo,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, ret_t.name)
    for {
      arg_tExp <- arg_t.readback_val(ctx, ValUniverse)
      ret_tExpVal <- ret_t.apply(
        TheNeu(arg_t, NeuVar(fresh_name)))
      ret_tExp <- ret_tExpVal.readback_val(
        ctx.ext(fresh_name, Bind(arg_t)), ValUniverse)
    } yield Pi(fresh_name, arg_tExp, ret_tExp)
  }
}
