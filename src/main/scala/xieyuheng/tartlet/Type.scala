package xieyuheng..tartlet

trait Type extends Exp {
  /** Type Exp must provide infer method */

  /** Type Exp have default check method */
  /*
   ctx :- exp => E
   ctx :- conversionCheck (UNIVERSE, T, E)
   -----------------
   ctx :- exp <= T
   */
  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    for {
      the <- infer(ctx)
      t2 <- the.t.eval(ctx.toEnv)
      _ok <- Util.conversionCheck(ctx, ValueUniverse, t, t2)
    } yield the.value
  }
}
